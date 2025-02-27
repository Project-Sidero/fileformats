/**
https://json5.org/
*/
module sidero.fileformats.json5;
public import sidero.fileformats.json;
import sidero.eventloop.coroutine;
import sidero.eventloop.coroutine.future_completion;
import sidero.eventloop.filesystem;
import sidero.eventloop.control;
import sidero.base.containers.map.hashmap;
import sidero.base.containers.list.linkedlist;
import sidero.base.containers.dynamicarray;
import sidero.base.containers.readonlyslice;
import sidero.base.containers.appender;
import sidero.base.math.bigint;
import sidero.base.allocators;
import sidero.base.allocators.classes;
import sidero.base.text;
import sidero.base.text.processing.errors;
import sidero.base.path.file;
import sidero.base.errors;

export @safe nothrow @nogc:

///
Future!JSONValue readJSON5(FilePath filePath, ErrorSinkRef errorSink) @trusted {
    static struct State {
        File file;
        Appender!char buffer;
        Future!(Slice!ubyte) nextChunk;
        ErrorSinkRef errorSink;

    @safe nothrow @nogc:

        this(File file, ErrorSinkRef errorSink) {
            this.file = file;
            this.errorSink = errorSink;
        }

        ~this() {
        }
    }

    enum Stages {
        OnStart,
        NextChunk
    }

    alias Builder = CoroutineBuilder!(State, Stages, JSONValue, File, ErrorSinkRef);
    Builder builder;

    builder[Stages.OnStart] = (scope ref state) @trusted {
        state.nextChunk = state.file.readChunk();
        assert(!state.nextChunk.isNull);

        // workaround for: https://issues.dlang.org/show_bug.cgi?id=23835
        auto ret = Builder.nextStage(Stages.NextChunk).after(state.nextChunk);
        return ret;
    };

    builder[Stages.NextChunk] = (scope ref state) @trusted {
        auto result = state.nextChunk.result;

        if(!result) {
            if(state.file.isReadEOF()) {
                JSONValue jsonValue;
                if(parseJSON5(state.file.path.toString(), state.buffer.asReadOnly, state.errorSink, jsonValue))
                    return Builder.complete(jsonValue);
                else
                    return Builder.complete(MalformedInputException("Not valid JSON5"));
            }

            return Builder.complete(result.getError());
        }

        state.buffer ~= cast(const(char)[])result.unsafeGetLiteral();

        {
            state.nextChunk = state.file.readChunk();
            assert(!state.nextChunk.isNull);

            // workaround for: https://issues.dlang.org/show_bug.cgi?id=23835
            auto ret = Builder.nextStage(Stages.NextChunk).after(state.nextChunk);
            return ret;
        }
    };

    auto theFile = File.from(filePath, true);
    if(!theFile) {
        InstanceableCoroutine!(JSONValue, FutureTriggerStorage!(JSONValue)**) instantiable = acquireInstantiableFuture!(JSONValue)();
        FutureTriggerStorage!(JSONValue)* triggerStorage;
        Future!(JSONValue) future = instantiable.makeInstance(RCAllocator.init, &triggerStorage);

        auto errorResult = completeWithoutATrigger(future, triggerStorage, theFile.getError().info);
        assert(errorResult);

        return future;
    }

    auto got = builder.build();
    assert(got);

    auto ret = got.makeInstance(RCAllocator.init, theFile, errorSink);
    registerAsTask(ret);
    return ret;
}

///
GenericCoroutine writeJSON5(FilePath fileName, JSONValue jsonValue) {
    StringBuilder_UTF8 builder = emitJSON5(jsonValue);
    String_UTF8 str = builder.asReadOnly;
    return write(fileName, str.asRawSlice);
}

///
bool parseJSON5(FilePath filePath, String_UTF8 contents, out StringBuilder_UTF8 messages, out JSONValue jsonValue) {
    ErrorSinkRef_StringBuilder errorSink = ErrorSinkRef_StringBuilder.make;

    bool ret = parseJSON5(filePath.toString(), contents, cast(ErrorSinkRef)errorSink, jsonValue);

    messages = errorSink.builder;
    return ret;
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
            if(errorSink.haveError)
                return JSONValue.init;
            else if(token.type == Token.Type.EndOfFile) {
                errorSink.error(token.loc, "Expecting a token seen end of file");
                return typeof(return).init;
            }

            JSONValue ret = JSONValue.create(JSONValue.Type.Null);
            DynamicArray!String_UTF8 comments;

            while(token.type == Token.Type.HashBangComment || token.type == Token.Type.SingleLineComment ||
                    token.type == Token.Type.MultiLineComment) {
                if(comments.isNull)
                    comments = ret.attachedComments;

                comments ~= token.text;
                lexer.popFront;

                token = lexer.token;
                if(errorSink.haveError)
                    return typeof(return).init;
                else if(token.type == Token.Type.EndOfFile) {
                    errorSink.error(token.loc, "Expecting a token seen end of file");
                    return typeof(return).init;
                }
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

                    if(token.punctuation == '+') {
                        if(nextIsNumber) {
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
                        } else if(nextTokenType == Token.Type.Identifier) {
                            token = lexer.token2;

                            if(token.text.ptr is Keywords.infinity.ptr) {
                                lexer.popFront;
                                lexer.popFront;

                                ret = double.infinity;
                                return ret;
                            }
                        }
                    } else if(token.punctuation == '-') {
                        if(nextIsNumber) {
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
                        } else if(nextTokenType == Token.Type.Identifier) {
                            token = lexer.token2;

                            if(token.text.ptr is Keywords.infinity.ptr) {
                                lexer.popFront;
                                lexer.popFront;

                                ret = -double.infinity;
                                return ret;
                            }
                        }
                    } else if(token.punctuation == '{') {
                        lexer.popFront;

                        ret = parseObject();
                        return ret;
                    } else if(token.punctuation == '[') {
                        lexer.popFront;

                        ret = parseArray();
                        return ret;
                    }

                    errorSink.error(token.loc, "JSON5 does not support the punctuation: {:s}", token.punctuation);

                    if(token.punctuation == '+' || token.punctuation == '-')
                        errorSink.errorSupplemental("A `+` or `-` must be followed by a number.");
                    return JSONValue.init;
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
                else if(token.type == Token.Type.EndOfFile) {
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
                DynamicArray!String_UTF8 comments;

                {
                    while(token.type == Token.Type.HashBangComment || token.type == Token.Type.SingleLineComment ||
                            token.type == Token.Type.MultiLineComment) {
                        comments ~= token.text;
                        lexer.popFront;

                        token = lexer.token;
                        if(errorSink.haveError)
                            return typeof(return).init;
                        else if(token.type == Token.Type.EndOfFile) {
                            errorSink.error(token.loc, "Expecting a comment or a string seen end of file");
                            return typeof(return).init;
                        }
                    }

                    if(token.type == Token.Type.Punctuation && token.punctuation == '}') {
                        lexer.popFront;
                        break;
                    } else if(token.type == Token.Type.String) {
                        key = token.text;
                        lexer.popFront;
                    } else {
                        errorSink.error(token.loc, "Unexpected token type {:s}, expected string for object member name", token.type);
                        return typeof(return).init;
                    }

                    token = lexer.token;
                    if(errorSink.haveError)
                        return typeof(return).init;
                    else if(token.type == Token.Type.EndOfFile) {
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
                    JSONValue jsonValue = this.parseJSONValue();
                    jsonValue.setOrAddAttachedComments(comments);

                    ret[key] = jsonValue;
                    if(errorSink.haveError)
                        return typeof(return).init;
                }

                {
                    token = lexer.token;
                    if(errorSink.haveError)
                        return typeof(return).init;
                    else if(token.type == Token.Type.EndOfFile) {
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

        head = Check.testSuccess("+Infinity");
        assert(head.type == JSONValue.Type.Number);

        head = Check.testSuccess("-Infinity");
        assert(head.type == JSONValue.Type.Number);

        head = Check.testSuccess("sometext");
        assert(head.type == JSONValue.Type.String);
    }

    {
        JSONValue head = Check.testSuccess("'my text'");
        assert(head.type == JSONValue.Type.String);
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

    {
        JSONValue head = Check.testSuccess("/* comment */{ /* another */ 'key': /* and here */ 123, 'another': 356 , }");
        assert(head.type == JSONValue.Type.Object);
    }
}

///
StringBuilder_UTF8 emitJSON5(JSONValue jsonValue) {
    StringBuilder_UTF8 builder;
    emitJSON5(builder, jsonValue);
    return builder;
}

///
void emitJSON5(ref StringBuilder_UTF8 builder, JSONValue jsonValue) {
    bool hasNewLines(String_UTF8 str) @safe nothrow @nogc {
        return str.contains("\n") || str.contains("\u2028") || str.contains("\u2029");
    }

    void emitPrefix(bool needed, int depth) @safe nothrow @nogc {
        if(!needed)
            return;

        foreach(i; 0 .. depth) {
            builder ~= "    ";
        }
    }

    void emitString(String_UTF8 str, int depth, bool requirePrefix) {
        emitPrefix(requirePrefix, depth);
        builder ~= "\"";

        const startOffset = builder.length;
        builder ~= str;

        builder[startOffset .. builder.length].replace("\\", "\\\\");
        builder[startOffset .. builder.length].replace("\"", "\\\"");

        builder ~= "\"";
    }

    void handle(JSONValue jsonValue, int depth, bool requirePrefix) @safe nothrow @nogc {
        foreach(comment; jsonValue.attachedComments()) {
            assert(comment);

            emitPrefix(requirePrefix, depth);

            if(hasNewLines(comment)) {
                builder ~= "/*";
                builder ~= comment;
                builder ~= "*/\n";
            } else {
                builder ~= "//";
                builder ~= comment.stripRight();
                builder ~= "\n";
            }

            requirePrefix = true;
        }

        jsonValue.match((LinkedList!JSONValue list) {
            emitPrefix(requirePrefix, depth);
            builder ~= "[\n";

            bool haveOne;

            foreach(value; list) {
                assert(value);
                if(haveOne)
                    builder ~= ",\n";

                haveOne = true;
                handle(value, depth + 1, true);
            }

            if(haveOne)
                builder ~= "\n";

            emitPrefix(requirePrefix, depth);
            builder ~= "]";
        }, (HashMap!(String_UTF8, JSONValue) map) {
            emitPrefix(requirePrefix, depth);
            builder ~= "{\n";

            bool haveOne;

            foreach(key, value; map) {
                assert(key);
                assert(value);

                if(haveOne)
                    builder ~= ",\n";

                haveOne = true;

                emitString(key, depth + 1, true);
                builder ~= ": ";
                handle(value, depth + 1, false);
            }

            if(haveOne)
                builder ~= "\n";

            emitPrefix(requirePrefix, depth);
            builder ~= "}";
        }, (DynamicBigInteger bigInteger) {
            emitPrefix(requirePrefix, depth);
            bigInteger.toString(builder);
            builder ~= "n";
        }, (bool b) { emitPrefix(requirePrefix, depth); builder ~= b ? "true" : "false"; }, (double number) {
            emitPrefix(requirePrefix, depth);
            builder.formattedWrite((number > 999_999 || number < 0.000_001) ? "{:e}" : "{:f}", number);
        }, (String_UTF8 text) { emitString(text, depth, requirePrefix); }, () {
            emitPrefix(requirePrefix, depth);
            builder ~= "null";
        });
    }

    handle(jsonValue, 0, false);
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

        void testSuccess(string mod = __MODULE__, int line = __LINE__)(string contents, string expected) {
            static immutable fileName = text(mod, ":", line);
            JSONValue jsonValue = testSuccess2(fileName, contents);
            StringBuilder_UTF8 builder = emitJSON5(jsonValue);

            if(builder != expected) {
                debugWriteln(jsonValue, builder, "!=", expected);
                assert(0);
            }
        }

        void testSuccess(string mod = __MODULE__, int line = __LINE__)(string contents, string expected1, string expected2) {
            static immutable fileName = text(mod, ":", line);
            JSONValue jsonValue = testSuccess2(fileName, contents);
            StringBuilder_UTF8 builder = emitJSON5(jsonValue);

            if(!(builder == expected1 || builder == expected2)) {
                debugWriteln(jsonValue, builder, "!=", expected1, " || ", expected2);
                assert(0);
            }
        }
    }

    {
        Check.testSuccess("'s\ne'", "\"s\ne\"");
        Check.testSuccess("'s\u2028e'", "\"s\u2028e\"");
    }

    {
        Check.testSuccess("['what', 'this']", "[\n    \"what\",\n    \"this\"\n]");
        Check.testSuccess("{'key1' : 123, 'key2': 456}", "{\n    \"key1\": 123,\n    \"key2\": 456\n}",
                "{\n    \"key2\": 456,\n    \"key1\": 123\n}");
    }

    {
        Check.testSuccess("[ /* what? */ 'thing' ]", "[\n    // what?\n    \"thing\"\n]");
    }
}
