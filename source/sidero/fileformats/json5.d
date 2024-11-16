module sidero.fileformats.json5;
public import sidero.fileformats.json;
import sidero.fileformats.errors;
import sidero.base.containers.map.hashmap;
import sidero.base.containers.list.linkedlist;
import sidero.base.allocators;
import sidero.base.allocators.classes;
import sidero.base.text;
import sidero.base.path.file;

export @safe nothrow @nogc:

///
bool parseJSON5(FilePath filePath, String_UTF8 contents, out StringBuilder_UTF8 messages, out JSONValue jsonValue) {
    ErrorSinkRef_StringBuilder errorSink = ErrorSinkRef_StringBuilder.make;

    bool ret = parseJSON5(filePath, contents, cast(ErrorSinkRef)errorSink, jsonValue);

    messages = errorSink.builder;
    return ret;
}

///
bool parseJSON5(FilePath filePath, String_UTF8 contents, ErrorSinkRef errorSink, out JSONValue jsonValue) {
    return parseJSON5(filePath.toString(), contents, errorSink, jsonValue);
}

///
bool parseJSON5(String_UTF8 fileName, String_UTF8 contents, ErrorSinkRef errorSink, out JSONValue jsonValue) {
    import sidero.fileformats.lexers.javascript;

    struct Parser {
        alias Keywords = Lexer_Javascript.Keywords;
        alias Token = Lexer_Javascript.Token;

        Lexer_Javascript lexer;
        ErrorSinkRef errorSink;

    @safe nothrow @nogc:

        this(Lexer_Javascript lexer, ErrorSinkRef errorSink) {
            this.lexer = lexer;
            this.errorSink = errorSink;
            assert(!this.errorSink.isNull);
        }

        /*
        JSON5Value:
            identifier null
            boolean an identifier true/false
            string
            number
            JSON5Object
            JSON5Array

        number:
            identifier NaN
            identifier Infinity
            Number
            BigInteger
            + number
            - number
        */
        JSONValue parseJSONValue() @trusted {
            if(lexer.empty)
                return JSONValue.create(JSONValue.Type.Null);

            Token token = lexer.token;
            JSONValue ret = JSONValue.create(JSONValue.Type.Null);

            {
                // TODO: consume comments
                token = lexer.token;
            }

            if(errorSink.haveError)
                return JSONValue.init;
            else if (token.type == Token.Type.EndOfFile) {
                errorSink.error(token.loc, "Expecting a token seen end of file");
                return typeof(return).init;
            }

            final switch(token.type) {
            case Token.Type.EndOfFile:
                break;

            case Token.Type.HashBangComment:
            case Token.Type.SingleLineComment:
            case Token.Type.MultiLineComment:
                break;

            case Token.Type.Identifier: {
                    if(token.text.ptr is Keywords.null_.ptr) {
                    } else if(token.text.ptr is Keywords.false_.ptr) {
                        ret = false;
                    } else if(token.text.ptr is Keywords.true_.ptr) {
                        ret = true;
                    } else if(token.text.ptr is Keywords.nan.ptr) {
                        ret = double.nan;
                    } else if(token.text.ptr is Keywords.infinity.ptr) {
                        ret = double.infinity;
                    } else {
                        ret = token.text;
                    }

                    lexer.popFront;
                    return ret;
                }

            case Token.Type.String:
                ret = token.text;
                lexer.popFront;
                return ret;

            case Token.Type.Punctuation: {
                    Token.Type nextTokenType = lexer.peek;
                    bool nextIsNumber = nextTokenType == Token.Type.Number || nextTokenType == Token.Type.BigInteger;

                    if(token.punctuation == '+' && nextIsNumber) {
                        lexer.popFront;

                        token = lexer.token;
                        if(errorSink.haveError)
                            return typeof(return).init;
                        lexer.popFront;

                        if(nextTokenType == Token.Type.Number)
                            ret = token.number;
                        else
                            ret = token.bigInteger;

                        return ret;
                    } else if(token.punctuation == '-' && nextIsNumber) {
                        lexer.popFront;

                        token = lexer.token;
                        if(errorSink.haveError)
                            return typeof(return).init;
                        lexer.popFront;

                        if(nextTokenType == Token.Type.Number)
                            ret = -token.number;
                        else
                            ret = -token.bigInteger;

                        return ret;
                    } else if(token.punctuation == '{') {
                        lexer.popFront;

                        ret = parseObject();
                        return ret;
                    } else if(token.punctuation == '[') {
                        lexer.popFront;

                        ret = parseArray();
                        return ret;
                    } else {
                        errorSink.error(token.loc, "JSON5 does not support the punctuation: {:s}", token.punctuation);

                        if(token.punctuation == '+' || token.punctuation == '-')
                            errorSink.errorSupplimental("A `+` or `-` must be followed by a number.");
                        return JSONValue.init;
                    }
                }

            case Token.Type.Number:
                ret = token.number;
                lexer.popFront;
                return ret;

            case Token.Type.BigInteger:
                ret = token.bigInteger;
                lexer.popFront;
                return ret;

            case Token.Type.RegexString:
                assert(0);

            case Token.Type.TemplateSubstitutionHead:
            case Token.Type.TemplateSubstitutionMiddle:
            case Token.Type.TemplateSubstitutionTail:
                errorSink.error(token.loc, "Template substitution strings are not supported in JSON5");
                break;
            }

            assert(0);
        }

        /*
        JSON5Object:
            { }
            { JSON5MemberList ,|opt }
        JSON5MemberList:
            JSON5Member
            JSON5MemberList , JSON5Member
        JSON5Member:
            JSON5MemberName : JSON5Value
        JSON5MemberName:
            identifier
            string
        */
        HashMap!(String_UTF8, JSONValue) parseObject() @trusted {
            // { already consumed

            HashMap!(String_UTF8, JSONValue) ret = HashMap!(String_UTF8, JSONValue)(globalAllocator());

            for(;;) {
                Token token = lexer.token;
                if(errorSink.haveError)
                    return typeof(return).init;
                else if (token.type == Token.Type.EndOfFile) {
                    errorSink.error(token.loc, "Expecting a punctuation `:`, `}` or a string seen end of file");
                    return typeof(return).init;
                }

                if(token.type == Token.Type.Punctuation) {
                    if(token.punctuation == ',') {
                        lexer.popFront;
                        continue;
                    } else if(token.punctuation == '}') {
                        lexer.popFront;
                        break;
                    }
                }

                String_UTF8 key;

                {
                    // TODO: key comments???

                    if(token.type == Token.Type.String) {
                        key = token.text;
                        lexer.popFront;
                    } else {
                        errorSink.error(token.loc, "Unexpected token type {:s}, expected string for object member name", token.type);
                        return typeof(return).init;
                    }

                    token = lexer.token;
                    if(errorSink.haveError)
                        return typeof(return).init;
                    else if (token.type == Token.Type.EndOfFile) {
                        errorSink.error(token.loc, "Expecting a punctuation `:` seen end of file");
                        return typeof(return).init;
                    }

                    if(token.type == Token.Type.Punctuation) {
                        if(token.punctuation == ':') {
                            lexer.popFront;
                        } else {
                            errorSink.error(token.loc, "Unexpected punctuation of {:s}, expected `:`", token.punctuation);
                            return typeof(return).init;
                        }
                    } else {
                        errorSink.error(token.loc, "Unexpected token {:s} expected a punctuation of `:`", token.type);
                        return typeof(return).init;
                    }
                }

                {
                    ret[key] = this.parseJSONValue();
                    if(errorSink.haveError)
                        return typeof(return).init;
                }

                {
                    token = lexer.token;
                    if(errorSink.haveError)
                        return typeof(return).init;
                    else if (token.type == Token.Type.EndOfFile) {
                        errorSink.error(token.loc, "Expecting a punctuation `,` or `}`, or a json value seen end of file");
                        return typeof(return).init;
                    }

                    if(token.type == Token.Type.Punctuation) {
                        if(token.punctuation == ',') {
                            lexer.popFront;
                        } else if(token.punctuation == '}') {
                            lexer.popFront;
                            break;
                        } else {
                            errorSink.error(token.loc, "Unexpected punctuation of {:s}, expected `,` or `}`", token.punctuation);
                            return typeof(return).init;
                        }
                    } else {
                        errorSink.error(token.loc, "Unexpected token {:s} expected a punctuation of `,` or `}`", token.type);
                        return typeof(return).init;
                    }
                }
            }

            return ret;
        }

        /*
        JSON5Array:
            [ ]
            [ JSON5ElementList ,|opt ]
        JSON5ElementList:
            JSON5Value
            JSON4ElementList , JSON5Value
        */
        LinkedList!JSONValue parseArray() {
            // [ already consumed
            LinkedList!JSONValue ret = LinkedList!JSONValue(globalAllocator());

            for(;;) {
                Token token = lexer.token;

                if(token.type == Token.Type.Punctuation) {
                    if(token.punctuation == ',') {
                        lexer.popFront;
                        continue;
                    } else if(token.punctuation == ']') {
                        lexer.popFront;
                        break;
                    } else {
                        errorSink.error(token.loc, "Unexpected punctuation of {:s}, expected `,` or `]`", token.punctuation);
                        return typeof(return).init;
                    }
                }

                {
                    ret ~= this.parseJSONValue();
                    if(errorSink.haveError)
                        return typeof(return).init;
                }

                {
                    token = lexer.token;

                    if(token.type == Token.Type.Punctuation) {
                        if(token.punctuation == ',') {
                            lexer.popFront;
                        } else if(token.punctuation == ']') {
                            lexer.popFront;
                            break;
                        } else {
                            errorSink.error(token.loc, "Unexpected punctuation of {:s}, expected `,` or `]`", token.punctuation);
                            return typeof(return).init;
                        }
                    } else {
                        errorSink.error(token.loc, "Unexpected token {:s} expected a punctuation of `,` or `]`", token.type);
                        return typeof(return).init;
                    }
                }
            }

            return ret;
        }
    }

    if(errorSink.isNull)
        return false;

    Parser parser = Parser(Lexer_Javascript(fileName, contents, errorSink), errorSink);
    jsonValue = parser.parseJSONValue();

    return !errorSink.haveError;
}

///
unittest {
    import sidero.base.console;
    import std.conv : text;

    struct Check {
    static @safe nothrow @nogc:
        JSONValue testSuccess2(string fileName, string contents) {
            ErrorSinkRef_StringBuilder errorSink = ErrorSinkRef_StringBuilder.make;
            assert(!errorSink.isNull);
            ErrorSinkRef errorSink2 = cast(ErrorSinkRef)errorSink;
            assert(!errorSink2.isNull);

            JSONValue jsonValue;

            if(!parseJSON5(String_UTF8(fileName), String_UTF8(contents), errorSink2, jsonValue)) {
                writeln(errorSink.builder);
                assert(0);
            }

            return jsonValue;
        }

        JSONValue testSuccess(string mod = __MODULE__, int line = __LINE__)(string contents) {
            static immutable fileName = text(mod, ":", line);
            return testSuccess2(fileName, contents);
        }
    }

    {
        JSONValue head = Check.testSuccess("");
        assert(head.type == JSONValue.Type.Null);
    }

    {
        JSONValue head = Check.testSuccess("false");
        assert(head.type == JSONValue.Type.Boolean);

        head = Check.testSuccess("true");
        assert(head.type == JSONValue.Type.Boolean);

        head = Check.testSuccess("NaN");
        assert(head.type == JSONValue.Type.Number);

        head = Check.testSuccess("Infinity");
        assert(head.type == JSONValue.Type.Number);

        head = Check.testSuccess("sometext");
        assert(head.type == JSONValue.Type.Text);
    }

    {
        JSONValue head = Check.testSuccess("'my text'");
        assert(head.type == JSONValue.Type.Text);
    }

    {
        JSONValue head = Check.testSuccess("42");
        assert(head.type == JSONValue.Type.Number);
        head = Check.testSuccess("+42");
        assert(head.type == JSONValue.Type.Number);
        head = Check.testSuccess("-42");
        assert(head.type == JSONValue.Type.Number);

        head = Check.testSuccess("32n");
        assert(head.type == JSONValue.Type.BigInteger);
        head = Check.testSuccess("+32n");
        assert(head.type == JSONValue.Type.BigInteger);
        head = Check.testSuccess("-32n");
        assert(head.type == JSONValue.Type.BigInteger);
    }

    {
        JSONValue head = Check.testSuccess("[]");
        assert(head.type == JSONValue.Type.Array);

        head = Check.testSuccess("[ , ]");
        assert(head.type == JSONValue.Type.Array);

        head = Check.testSuccess("[ abc ]");
        assert(head.type == JSONValue.Type.Array);

        head = Check.testSuccess("[ 123, 356 , ]");
        assert(head.type == JSONValue.Type.Array);
    }

    {
        JSONValue head = Check.testSuccess("{}");
        assert(head.type == JSONValue.Type.Object);

        head = Check.testSuccess("{ , }");
        assert(head.type == JSONValue.Type.Object);

        head = Check.testSuccess("{ 'abc' : 'def' }");
        assert(head.type == JSONValue.Type.Object);

        head = Check.testSuccess("{ 'key': 123, 'another': 356 , }");
        assert(head.type == JSONValue.Type.Object);
    }
}
