module sidero.fileformats.lexers.javascript;
import sidero.fileformats.lexers.interning;
import sidero.fileformats.lexers.defs;
import sidero.base.text;
import sidero.base.text.unicode.characters.uax31;
import sidero.base.math.bigint;

/+
Tokens:
- Identifier https://262.ecma-international.org/15.0/index.html#prod-IdentifierName
    Start Continue*
    Start: ID_Start | '_' | '$' | '\' unicode escape sequence
    Continue: ID_Continue | '$' | \U200C | \U200D | '\' unicode escape sequence
    \uFFFF | \u{FFFFFF} https://262.ecma-international.org/15.0/index.html#prod-UnicodeEscapeSequence
- Punctuation
    { } [ ] : ,
- String
    " double string characters "
    ' single string characters '
    \U2028 \U2029
    \ EscapeSequence
        \' \" \\ \b \f \n \r \t \v \0
        27 22 5c 08 0c 0a 0d 09 0b 00
    any unicode character
    \ u XXXX
        is a UTF-16 code unit
- Number
    https://262.ecma-international.org/15.0/index.html#prod-NumericLiteral

    Separator: _

    Binary:
        0b Digits
        0B Digits
        Digits: [0-1] (Separator|opt [0-1] Digits|opt)|opt

    Octal:
        0o Digits
        0O Digits
        Digits: [0-7] (Separator|opt [0-7] Digits|opt)|opt

    Decimal:
        0 FloatPart|opt
        [1-9] Digits FloatPart|opt
        [1-9] Digits n|opt (becomes big integer)
        Digits: (Separator|opt [0-9] Digits|opt)|opt

    SignedDecimal:
        + Digits FloatPart|opt
        - Digits FloatPart|opt
        Digits: [0-9] (Separator|opt [0-9] Digits|opt)|opt

    Hex:
        0x Digits
        0X Digits
        Digits: [0-9a-fA-F] (Separator|opt [0-9a-fA-F] Digits|opt)|opt

    FloatPart:
        . PartDigits ExponentPart|opt
        PartDigits: [0-9] (Separator|opt [0-9] PartDigits|opt)|opt
        ExponentPart: e ExponentInteger
        ExponentPart: E ExponentInteger
        ExponentInteger: PartDigits
        ExponentInteger: + PartDigits
        ExponentInteger: - PartDigits

- Comment
    /* any */
    // any

- template literal

+/

/**
For identifiers is UAX31 compliant.

For numbers, negation is not handled due to not having the ability to determine between unary and binary.
*/
struct Lexer_Javascript {
    private {
        import sidero.fileformats.internal.smallarray;

        Token[3] tokens;

        Lexer_Interning interning;
        String_UTF8 contentsStorage;

        Loc currentLocation;

        const(char)* startOfFile, currentCharacter, endOfFile;
        ushort haveTokens;

        bool hitError;
        bool inTemplate, inTemplateSubstitution;
    }

    ///
    static struct Token {
        ///
        Loc loc;
        ///
        Type type;

        ///
        String_UTF8 text;
        ///
        DynamicBigInteger bigInteger;

        ///
        union {
            ///
            double number;

            ///
            char punctuation;

            ///
            dchar character;
        }

        ///
        enum Type {
            ///
            EndOfFile,
            ///
            HashBangComment,
            ///
            SingleLineComment,
            ///
            MultiLineComment,
            ///
            Identifier,
            ///
            Punctuation,
            ///
            Character,
            ///
            String,
            ///
            TemplateSubstitutionHead,
            ///
            TemplateSubstitutionMiddle,
            ///
            TemplateSubstitutionTail,
            ///
            Number,
            ///
            BigInteger,
        }

    export @safe nothrow @nogc:

        this(return scope ref Token other) scope {
            this.tupleof = other.tupleof;
        }

        ~this() scope {
        }

        void opAssign(return scope Token other) scope {
            this.destroy;
            this.__ctor(other);
        }
    }

export @safe nothrow @nogc:

    ///
    this(String_UTF8 fileName, String_UTF8 contents) scope @trusted {
        if(!contents.isPtrNullTerminated)
            contents = contents.dup;

        this.currentLocation = Loc(fileName, 1, 1);
        this.contentsStorage = contents;

        auto text = contents.unsafeGetLiteral();
        if(text !is null) {
            startOfFile = text.ptr;
            currentCharacter = startOfFile;
            endOfFile = text.ptr + text.length;
        }

        foreach(k; __traits(allMembers, Keywords)) {
            this.interning.preFill(__traits(getMember, Keywords, k));
        }
    }

    this(return scope ref Lexer_Javascript other) scope @trusted {
        foreach(i, t; other.tokens) {
            this.tokens[i] = t;
        }

        this.tupleof[1 .. $] = other.tupleof[1 .. $];
    }

    ~this() scope {
    }

    ///
    enum Keywords : String_UTF8 {
        await = String_UTF8("await"), ///
        break_ = String_UTF8("break"), ///
        case_ = String_UTF8("case"), ///
        catch_ = String_UTF8("catch"), ///
        class_ = String_UTF8("class"), ///
        const_ = String_UTF8("const"), ///
        continue_ = String_UTF8("continue"), ///
        debugger = String_UTF8("debugger"), ///
        default_ = String_UTF8("default"), ///
        delete_ = String_UTF8("delete"), ///
        do_ = String_UTF8("do"), ///
        else_ = String_UTF8("else"), ///
        enum_ = String_UTF8("enum"), ///
        export_ = String_UTF8("export"), ///
        extends = String_UTF8("extends"), ///
        false_ = String_UTF8("false"), ///
        finally_ = String_UTF8("finally"), ///
        for_ = String_UTF8("for"), ///
        function_ = String_UTF8("function"), ///
        if_ = String_UTF8("if"), ///
        import_ = String_UTF8("import"), ///
        in_ = String_UTF8("in"), ///
        instanceof = String_UTF8("instanceof"), ///
        new_ = String_UTF8("new"), ///
        null_ = String_UTF8("null"), ///
        return_ = String_UTF8("return"), ///
        super_ = String_UTF8("super"), ///
        switch_ = String_UTF8("switch"), ///
        this_ = String_UTF8("this"), ///
        throw_ = String_UTF8("throw"), ///
        true_ = String_UTF8("true"), ///
        try_ = String_UTF8("try"), ///
        typeof_ = String_UTF8("typeof"), ///
        var = String_UTF8("var"), ///
        void_ = String_UTF8("void"), ///
        while_ = String_UTF8("while"), ///
        with_ = String_UTF8("with"), ///
        yield = String_UTF8("yield"), ///
    }

    ///
    bool isNull() scope const {
        return this.currentLocation.fileName.isNull;
    }

    ///
    bool empty() scope const {
        return front() == Token.Type.EndOfFile;
    }

    ///
    Token.Type front() scope const {
        assert(this.haveTokens > 0);
        return tokens[0].type;
    }

    ///
    void popFront() scope @trusted {
        if(this.haveTokens > 1) {
            foreach(i, v; this.tokens) {
                this.tokens[i - 1] = v;
            }

            this.haveTokens--;
        } else {
            this.haveTokens--;
            fillToken(0);
        }
    }

    ///
    Token token() return scope {
        fillToken(0);
        assert(this.haveTokens > 0);
        return this.tokens[0];
    }

    ///
    Token.Type peek() scope {
        fillToken(0);
        fillToken(1);
        assert(this.haveTokens > 1);
        return this.tokens[1].type;
    }

    ///
    Token.Type peek2() scope {
        fillToken(0);
        fillToken(1);
        fillToken(2);
        assert(this.haveTokens > 2);
        return this.tokens[2].type;
    }

private:
    void fillToken(size_t tokenOffset) scope {
        if(this.haveTokens > tokenOffset)
            return;

        this.tokens[tokenOffset] = Token.init;

        if(tokenOffset == 0) {
            scan(this.tokens[0]);
            this.haveTokens = 1;
        } else {
            assert(this.haveTokens == tokenOffset);
            scan(this.tokens[this.haveTokens]);
            this.haveTokens++;
        }
    }

    void scan(scope ref Token token) scope @trusted {
        if(this.inTemplate) {
            // https://262.ecma-international.org/15.0/index.html#prod-TemplateSubstitutionTail

            if(this.inTemplateSubstitution) {
                if(*currentCharacter == '}') {
                    // } ends scanning for js tokens
                    this.inTemplateSubstitution = false;
                    currentCharacter++;
                    this.currentLocation.lineOffset++;
                } else
                    return scanActual(token);
            }

            // TODO: lex template substitution tail (or middle if ends with ${)

            // end of ${
            this.token.type = Token.Type.TemplateSubstitutionMiddle;

            // end of `
            this.token.type = Token.Type.TemplateSubstitutionTail;

        } else
            scanActual(token);
    }

    void scanActual(scope ref Token token) scope @trusted {
        import sidero.base.encoding.utf : decode, decodeLength;
        import sidero.base.text.unicode.characters.database;

        enum MaxNumber = 0xFFFFFFFFFFFFF;

        token.type = Token.Type.EndOfFile;

        if(this.hitError)
            return;

        while(currentCharacter < endOfFile) {
            char asciiChar = *currentCharacter;
            currentCharacter++;

            token.loc = this.currentLocation;
            this.currentLocation.lineOffset++;

            switch(asciiChar) {
            case '\n':
                this.currentLocation.lineNumber++;
                this.currentLocation.lineOffset = 1;
                break;

            case '\r':
            case ' ':
            case '\u00A0':
            case '\u0009':
            case '\u000B': .. case '\u000C':
                // ignore
                break;

            case '!':
            case '%': .. case '&':
            case '(': .. case ',':
            case ':': .. case '?':
            case '[':
            case ']': .. case '^':
            case '{': .. case '~':
                token.type = Token.Type.Punctuation;
                token.punctuation = asciiChar;
                return;

            case '_':
            case '$':
            case 'a': .. case 'z':
            case 'A': .. case 'Z':
                lexIdentifier(token, currentCharacter - 1, 0, 0, 0);
                return;

            case '\\': {
                    const(char)* startOfToken = currentCharacter - 1;
                    size_t escape4, escape6, escape6Count;

                    if(*currentCharacter == 'u') {
                        currentCharacter++;

                        dchar temp = 0;

                        if(*currentCharacter == '{') {
                            // escape 6 is a code point
                            this.currentCharacter++;
                            this.currentLocation.lineOffset++;

                            static foreach(i; 0 .. 6) {
                                if(currentCharacter + 1 >= endOfFile || *currentCharacter == '}')
                                    goto DoneEscapeIdentifier6;

                                static if(i > 0)
                                    temp <<= 4;

                                if(*currentCharacter >= 'a' && *currentCharacter <= 'f')
                                    temp |= ((*currentCharacter) - 'a') + 10;
                                else if(*currentCharacter >= 'A' && *currentCharacter <= 'F')
                                    temp |= ((*currentCharacter) - 'A') + 10;
                                else if(*currentCharacter >= '0' && *currentCharacter <= '9')
                                    temp |= (*currentCharacter) - '0';
                                else {
                                    goto EscapeIdentifierError;
                                }

                                currentCharacter++;
                                escape6++;
                                this.currentLocation.lineOffset++;
                            }
            DoneEscapeIdentifier6:

                            if(currentCharacter >= endOfFile || *currentCharacter != '}')
                                goto EscapeIdentifierError;

                            currentCharacter++;
                            escape6Count++;
                            escape6 += 4;
                            this.currentLocation.lineOffset++;

                            if(isUAX31_Javascript_Start(temp)) {
                                lexIdentifier(token, startOfToken, escape4, escape6, escape6Count);
                                return;
                            } else
                                goto EscapeIdentifierError;
                        } else {
                            // escape 4 is a UTF-16 code unit
                            // at this level we don't need to care about surrogates

                            static foreach(i; 0 .. 4) {
                                static if(i > 0)
                                    temp <<= 4;

                                if(*currentCharacter >= 'a' && *currentCharacter <= 'f')
                                    temp |= ((*currentCharacter) - 'a') + 10;
                                else if(*currentCharacter >= 'A' && *currentCharacter <= 'F')
                                    temp |= ((*currentCharacter) - 'A') + 10;
                                else if(*currentCharacter >= '0' && *currentCharacter <= '9')
                                    temp |= (*currentCharacter) - '0';
                                else
                                    goto EscapeIdentifierError;

                                currentCharacter++;
                            }

                            this.currentLocation.lineOffset += 4;
                            escape4 += 6;

                            if(temp >= 0xD800 && temp <= 0xDBFF) {
                                // this is a UTF-16 surrogate

                                wchar[2] temp2 = [cast(wchar)temp, 0];
                                char c;

                                if(!(*currentCharacter == '\\' && *(currentCharacter + 1) == 'u'))
                                    goto EscapeSurrogateError;
                                else if(*(currentCharacter + 2) == '{')
                                    goto EscapeSurrogateError;

                                static foreach(i; 0 .. 4) {
                                    static if(i > 0)
                                        temp2[1] <<= 4;

                                    c = *(currentCharacter + i + 2);

                                    if(c >= 'a' && c <= 'z')
                                        temp2[1] |= (c - 'a') + 10;
                                    else if(c >= 'A' && c <= 'Z')
                                        temp2[1] |= (c - 'A') + 10;
                                    else if(c >= '0' && c <= '9')
                                        temp2[1] |= c - '0';
                                    else
                                        assert(0, "Handle identifier escape 4 ICE");
                                }

                                currentCharacter += 6;
                                escape4 += 6;

                                decode(temp2, temp);
                            }

                            if(isUAX31_Javascript_Start(temp)) {
                                lexIdentifier(token, startOfToken, escape4, escape6, escape6Count);
                                return;
                            } else
                                goto EscapeIdentifierError;
                        }
                    } else
                        goto EscapeIdentifierError;

            EscapeIdentifierError:
                    // TODO: error
                    return;

            EscapeSurrogateError:
                    // TODO: error
                    return;
                }

            case '0': {
                    char next = *currentCharacter;

                    if(next == 'b' || next == 'B') {
                        // binary
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        lexBinaryNumber(token);
                    } else if(next == 'o' || next == 'O') {
                        // octal
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        lexOctalNumber(token);
                    } else if(next == 'x' || next == 'X') {
                        // hex
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        lexHexNumber(token);
                    } else {
                        // dec
                        lexDecimalNumber(token);
                    }

                    return;
                }

            case '1': .. case '9': {
                    // dec
                    currentCharacter--;
                    this.currentLocation.lineOffset--;
                    lexDecimalNumber(token);
                    return;
                }

            case '.': {
                    if(*currentCharacter >= '0' && *currentCharacter <= '9') {
                        currentCharacter--;
                        this.currentLocation.lineOffset--;
                        lexDecimalNumber(token);
                    } else {
                        token.type = Token.Type.Punctuation;
                        token.punctuation = asciiChar;
                    }

                    return;
                }

            case '#': {

                    if(*currentCharacter == '!') {
                        // single line comment
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        while((*currentCharacter) == ' ') {
                            // won't bother with other white space
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                        }

                        const(char*) startOfToken = this.currentCharacter;

                        while(currentCharacter < endOfFile) {
                            asciiChar = *currentCharacter;

                            if(asciiChar == '\n' || (asciiChar == '\r' && *(currentCharacter + 1) == '\n'))
                                break;
                            else if(asciiChar > 127) {
                                size_t consumed, offset;
                                dchar decoded = decode(() @trusted { return currentCharacter + offset >= endOfFile; }, () @trusted {
                                    return cast(char)*(currentCharacter + offset);
                                }, () { offset++; }, consumed);

                                if(decoded == '\u2028' || decoded == '\u2029')
                                    break;

                                this.currentLocation.lineOffset++;
                                currentCharacter += consumed;
                                continue;
                            }

                            this.currentLocation.lineOffset++;
                            currentCharacter++;
                        }

                        token.type = Token.Type.HashBangComment;
                        token.text = String_UTF8(startOfToken[0 .. currentCharacter - startOfToken]).dup;
                        return;
                    }

                    // TODO: error
                    return;
                }

            case '/': {
                    if(*currentCharacter == '*') {
                        // multiline comment

                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        const(char*) startOfToken = this.currentCharacter;
                        uint endChars;

                        while(currentCharacter < endOfFile) {
                            asciiChar = *currentCharacter;

                            if(asciiChar == '*') {
                                char next = *(currentCharacter + 1);

                                if(next == '/') {
                                    this.currentLocation.lineOffset += 2;
                                    currentCharacter += 2;
                                    endChars = 2;
                                    break;
                                }
                            }

                            if(asciiChar == '\n' || (asciiChar == '\r' && *(currentCharacter + 1) == '\n')) {
                                this.currentLocation.lineNumber++;
                                this.currentLocation.lineOffset = 1;
                            } else if(asciiChar > 127) {
                                size_t consumed, offset;
                                dchar decoded = decode(() @trusted { return currentCharacter + offset >= endOfFile; }, () @trusted {
                                    return cast(char)*(currentCharacter + offset);
                                }, () { offset++; }, consumed);

                                if(decoded == '\u2028' || decoded == '\u2029') {
                                    this.currentLocation.lineNumber++;
                                    this.currentLocation.lineOffset = 1;
                                }

                                this.currentLocation.lineOffset++;
                                currentCharacter += consumed;
                                continue;
                            }

                            this.currentLocation.lineOffset++;
                            currentCharacter++;
                        }

                        token.type = Token.Type.MultiLineComment;
                        token.text = String_UTF8(startOfToken[0 .. (currentCharacter - endChars) - startOfToken]).dup;
                        return;
                    } else if(*currentCharacter == '/') {
                        // single line comment
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        while((*currentCharacter) == ' ') {
                            // won't bother with other white space
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                        }

                        const(char*) startOfToken = this.currentCharacter;

                        while(currentCharacter < endOfFile) {
                            asciiChar = *currentCharacter;

                            if(asciiChar == '\n' || (asciiChar == '\r' && *(currentCharacter + 1) == '\n'))
                                break;
                            else if(asciiChar > 127) {
                                size_t consumed, offset;
                                dchar decoded = decode(() @trusted { return currentCharacter + offset >= endOfFile; }, () @trusted {
                                    return cast(char)*(currentCharacter + offset);
                                }, () { offset++; }, consumed);

                                if(decoded == '\u2028' || decoded == '\u2029')
                                    break;

                                this.currentLocation.lineOffset++;
                                currentCharacter += consumed;
                                continue;
                            }

                            this.currentLocation.lineOffset++;
                            currentCharacter++;
                        }

                        token.type = Token.Type.SingleLineComment;
                        token.text = String_UTF8(startOfToken[0 .. currentCharacter - startOfToken]).dup;
                        return;
                    }

                    // punctuation
                    token.type = Token.Type.Punctuation;
                    token.punctuation = asciiChar;
                    return;
                }

            case '\'': {
                    asciiChar = *currentCharacter;

                    if(asciiChar == '\\') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        size_t consumed, given;

                        const(char)* startToken = currentCharacter;

                        if(!skipEscape(consumed, given))
                            goto CharacterLiteralEscapeError;

                        if(*currentCharacter != '\'')
                            goto CharacterLiteralError;
                        this.currentCharacter++;
                        this.currentLocation.lineOffset++;

                        token.type = Token.Type.Character;
                        token.character = parseEscape(startToken, null, consumed);
                    } else if(asciiChar == '\'') {
                        goto CharacterLiteralError;
                    } else {
                        const countChars = decodeLength(asciiChar);

                        if(countChars == 1 && *currentCharacter == '\n') {
                            this.currentLocation.lineNumber++;
                            this.currentLocation.lineOffset = 0;
                        } else if(countChars == 4 && *currentCharacter == 0xE2 && *(currentCharacter + 1) == 0x80 &&
                                (*(currentCharacter + 2) == 0xA8 || *(currentCharacter + 2) == 0xA9)) {
                            this.currentLocation.lineNumber++;
                            this.currentLocation.lineOffset = 0;
                        } else {
                            this.currentLocation.lineOffset++;
                        }

                        if(*(currentCharacter + countChars) != '\'')
                            goto CharacterLiteralError;

                        currentCharacter += countChars + 1;
                        token.type = Token.Type.Character;

                        if(countChars == 1) {
                            token.character = asciiChar;
                        } else {
                            size_t consumed, offset;
                            dchar decoded = decode(() @trusted { return currentCharacter + offset >= endOfFile; }, () @trusted {
                                return cast(char)*(currentCharacter + offset);
                            }, () { offset++; }, consumed);

                            token.character = decoded;
                        }
                    }

                    return;

            CharacterLiteralError:
                    // TODO: error
                    return;

            CharacterLiteralEscapeError:
                    // TODO: error
                    return;
                }

            case '\"': {
                    const(char)* startToken = currentCharacter;
                    size_t consumed, given;

                    while(currentCharacter < endOfFile) {
                        asciiChar = *currentCharacter;

                        if(asciiChar == '\\') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;

                            if(!skipEscape(consumed, given))
                                goto StringLiteralEscapeError;
                        } else if(asciiChar == '\"') {
                            break;
                        } else {
                            const countChars = decodeLength(asciiChar);

                            if(countChars == 1 && *currentCharacter == '\n') {
                                this.currentLocation.lineNumber++;
                                this.currentLocation.lineOffset = 0;
                            } else if(countChars == 4 && *currentCharacter == 0xE2 && *(currentCharacter + 1) == 0x80 &&
                                    (*(currentCharacter + 2) == 0xA8 || *(currentCharacter + 2) == 0xA9)) {
                                this.currentLocation.lineNumber++;
                                this.currentLocation.lineOffset = 0;
                            } else
                                this.currentLocation.lineOffset++;

                            currentCharacter += countChars;
                        }
                    }

                    if(*currentCharacter != '\"')
                        goto StringLiteralEscapeError;
                    currentCharacter++;
                    this.currentLocation.lineOffset++;

                    {
                        SmallArray!char array = SmallArray!char(((currentCharacter - startToken) - consumed) + given);
                        char[] buffer = array.get;
                        size_t usedOfBuffer;

                        const(char)* ptr = startToken;

                        while(ptr < endOfFile) {
                            asciiChar = *ptr;

                            if(asciiChar == '\\') {
                                ptr++;
                                this.currentLocation.lineOffset++;

                                dchar decoded = parseEscape(ptr, buffer, usedOfBuffer);
                            } else if(asciiChar == '\"') {
                                break;
                            } else {
                                const countChars = decodeLength(asciiChar);

                                if(countChars == 1 && *ptr == '\n') {
                                    this.currentLocation.lineNumber++;
                                    this.currentLocation.lineOffset = 0;
                                } else if(countChars == 4 && *ptr == 0xE2 && *(ptr + 1) == 0x80 && (*(ptr + 2) == 0xA8 || *(ptr + 2) == 0xA9)) {
                                    this.currentLocation.lineNumber++;
                                    this.currentLocation.lineOffset = 0;
                                } else
                                    this.currentLocation.lineOffset++;

                                foreach(i; 0 .. countChars) {
                                    buffer[usedOfBuffer++] = *(ptr + i);
                                }

                                ptr += countChars;
                            }
                        }

                        token.type = Token.Type.String;
                        token.text = String_UTF8(buffer[0 .. usedOfBuffer]).dup;
                        return;
                    }

            StringLiteralEscapeError:
                    // TODO: error
                    return;
                }

            default: {
                    currentCharacter--;
                    this.currentLocation.lineOffset--;

                    size_t consumed, offset;
                    dchar decoded = decode(() @trusted { return currentCharacter + offset >= endOfFile; }, () @trusted {
                        return cast(char)*(currentCharacter + offset);
                    }, () { offset++; }, consumed);

                    if(decoded == '\u2028' || decoded == '\u2029') {
                        this.currentLocation.lineNumber++;
                        this.currentLocation.lineOffset = 1;
                        continue;
                    } else if(decoded == '\uFEFF' || getGeneralCategory(decoded) == GeneralCategory.Zs) {
                        this.currentLocation.lineOffset++;
                        continue;
                    } else if(isUAX31_Javascript_Start(decoded)) {
                        this.currentCharacter += consumed;
                        this.currentLocation.lineOffset++;
                        lexIdentifier(token, currentCharacter - consumed, 0, 0, 0);
                        return;
                    }

                    // TODO: error
                    return;
                }
            }
        }
    }

    void lexIdentifier(scope ref Token token, scope const(char)* startOfToken, size_t escape4, size_t escape6, size_t escape6Count) scope @trusted {
        import sidero.base.encoding.utf : decode, encodeUTF8;

        token.type = Token.Type.Identifier;

        import sidero.base.console;

        while(currentCharacter < endOfFile) {
            char asciiChar = *currentCharacter;

            if((asciiChar >= 'a' && asciiChar <= 'z') || (asciiChar >= 'A' && asciiChar <= 'Z') || (asciiChar >= '0' &&
                    asciiChar <= '9') || asciiChar == '_') {
                this.currentCharacter++;
                this.currentLocation.lineOffset++;
                continue;
            } else if(asciiChar == '\\') {
                if(*(currentCharacter + 1) == 'u') {
                    this.currentCharacter += 2;
                    this.currentLocation.lineOffset += 2;

                    dchar temp = 0;

                    if(*currentCharacter == '{') {
                        // escape 6 is a code point
                        this.currentCharacter++;
                        this.currentLocation.lineOffset++;

                        static foreach(i; 0 .. 6) {
                            if(currentCharacter + 1 >= endOfFile || *currentCharacter == '}')
                                goto DoneEscapeIdentifier6;

                            static if(i > 0)
                                temp <<= 4;

                            if(*currentCharacter >= 'a' && *currentCharacter <= 'f')
                                temp |= ((*currentCharacter) - 'a') + 10;
                            else if(*currentCharacter >= 'A' && *currentCharacter <= 'F')
                                temp |= ((*currentCharacter) - 'A') + 10;
                            else if(*currentCharacter >= '0' && *currentCharacter <= '9')
                                temp |= (*currentCharacter) - '0';
                            else {
                                goto EscapeIdentifierError;
                            }

                            currentCharacter++;
                            escape6++;
                            this.currentLocation.lineOffset++;
                        }
                    DoneEscapeIdentifier6:

                        if(currentCharacter >= endOfFile || *currentCharacter != '}')
                            goto EscapeIdentifierError;

                        currentCharacter++;
                        escape6Count++;
                        escape6 += 4;
                        this.currentLocation.lineOffset++;

                        if(isUAX31_Javascript_Continue(temp))
                            continue;
                        else
                            goto EscapeIdentifierError;
                    } else {
                        // escape 4 is a UTF-16 code unit
                        // at this level we don't need to care about surrogates

                        static foreach(i; 0 .. 4) {
                            static if(i > 0)
                                temp <<= 4;

                            if(*currentCharacter >= 'a' && *currentCharacter <= 'f')
                                temp |= ((*currentCharacter) - 'a') + 10;
                            else if(*currentCharacter >= 'A' && *currentCharacter <= 'F')
                                temp |= ((*currentCharacter) - 'A') + 10;
                            else if(*currentCharacter >= '0' && *currentCharacter <= '9')
                                temp |= (*currentCharacter) - '0';
                            else
                                goto EscapeIdentifierError;

                            currentCharacter++;
                        }

                        this.currentLocation.lineOffset += 4;
                        escape4 += 6;

                        if(temp >= 0xD800 && temp <= 0xDBFF) {
                            // this is a UTF-16 surrogate

                            wchar[2] temp2 = [cast(wchar)temp, 0];
                            char c;

                            if(!(*currentCharacter == '\\' && *(currentCharacter + 1) == 'u'))
                                goto EscapeSurrogateError;
                            else if(*(currentCharacter + 2) == '{')
                                goto EscapeSurrogateError;

                            static foreach(i; 0 .. 4) {
                                static if(i > 0)
                                    temp2[1] <<= 4;

                                c = *(currentCharacter + i + 2);

                                if(c >= 'a' && c <= 'z')
                                    temp2[1] |= (c - 'a') + 10;
                                else if(c >= 'A' && c <= 'Z')
                                    temp2[1] |= (c - 'A') + 10;
                                else if(c >= '0' && c <= '9')
                                    temp2[1] |= c - '0';
                                else
                                    assert(0, "Handle identifier escape 4 ICE");
                            }

                            currentCharacter += 6;
                            escape4 += 6;

                            decode(temp2, temp);
                        }

                        if(isUAX31_Javascript_Continue(temp))
                            continue;
                        else
                            goto EscapeIdentifierError;
                    }
                } else
                    goto EscapeIdentifierError;

            EscapeIdentifierError:
                // TODO: error
                return;
            } else {
                size_t consumed, offset;
                dchar decoded = decode(() @trusted { return currentCharacter + offset >= (endOfFile + 1); }, () @trusted {
                    return cast(char)*(currentCharacter + offset);
                }, () { offset++; }, consumed);

                if(isUAX31_Javascript_Continue(decoded)) {
                    this.currentCharacter += consumed;
                    this.currentLocation.lineOffset++;
                    continue;
                }
            }

            break;
        }

        {
            const(char)[] slice = startOfToken[0 .. (currentCharacter - startOfToken)];

            if((escape4 | escape6) != 0) {
                const newLength = (slice.length - (escape4 + escape6)) + ((escape6Count + (escape4 / 6)) * 4);

                SmallArray!char array = SmallArray!char(newLength);
                char[] buffer = array.get;
                size_t usedOfBuffer;

                {
                    const(char)* ptr = slice.ptr;

                    while(ptr < slice.ptr + slice.length) {
                        if(*ptr == '\\' && *(ptr + 1) == 'u') {
                            dchar temp = 0;
                            char c;

                            if(*(ptr + 2) == '{') {
                                ptr += 3;

                                static foreach(i; 0 .. 6) {
                                    c = *ptr;

                                    if(c == '}' || c == '\0')
                                        goto Escape6DecodeDone;

                                    static if(i > 0)
                                        temp <<= 4;

                                    if(c >= 'a' && c <= 'f')
                                        temp |= (c - 'a') + 10;
                                    else if(c >= 'A' && c <= 'F')
                                        temp |= (c - 'A') + 10;
                                    else if(c >= '0' && c <= '9')
                                        temp |= c - '0';
                                    else
                                        assert(0, "Handle identifier escape 6 ICE");

                                    ptr++;
                                }
                            Escape6DecodeDone:

                                ptr++;
                            } else {
                                static foreach(i; 0 .. 4) {
                                    static if(i > 0)
                                        temp <<= 4;

                                    c = *(ptr + i + 2);

                                    if(c >= 'a' && c <= 'f')
                                        temp |= (c - 'a') + 10;
                                    else if(c >= 'A' && c <= 'F')
                                        temp |= (c - 'A') + 10;
                                    else if(c >= '0' && c <= '9')
                                        temp |= c - '0';
                                    else
                                        assert(0, "Handle identifier escape 4 ICE");
                                }

                                ptr += 6;

                                if(temp >= 0xD800 && temp <= 0xDBFF) {
                                    // this is a UTF-16 surrogate

                                    wchar[2] temp2 = [cast(wchar)temp, 0];

                                    if(!(*ptr == '\\' && *(ptr + 1) == 'u'))
                                        goto EscapeSurrogateError;
                                    else if(*(ptr + 2) == '{')
                                        goto EscapeSurrogateError;

                                    static foreach(i; 0 .. 4) {
                                        static if(i > 0)
                                            temp2[1] <<= 4;

                                        c = *(ptr + i + 2);

                                        if(c >= 'a' && c <= 'z')
                                            temp2[1] |= (c - 'a') + 10;
                                        else if(c >= 'A' && c <= 'Z')
                                            temp2[1] |= (c - 'A') + 10;
                                        else if(c >= '0' && c <= '9')
                                            temp2[1] |= c - '0';
                                        else
                                            assert(0, "Handle identifier escape 4 ICE");
                                    }

                                    if(temp2[1] < 0xDC00 || temp2[1] > 0xDFFF)
                                        goto EscapeSurrogateError;

                                    ptr += 6;
                                    decode(temp2, temp);
                                }
                            }

                            const encoded = encodeUTF8(temp, *cast(char[4]*)(buffer.ptr + usedOfBuffer));
                            usedOfBuffer += encoded;
                        } else {
                            buffer[usedOfBuffer] = *ptr;
                            ptr++;
                            usedOfBuffer++;
                        }
                    }
                }

                token.text = this.interning.store(buffer[0 .. usedOfBuffer], false);
            } else
                token.text = this.interning.store(slice, false);

            return;
        }

    EscapeSurrogateError:
        // TODO: errors
        return;
    }

    void lexBinaryNumber(ref Token token) @trusted {
        enum MaxNumber = 0xFFFFFFFFFFFFF;

        // max 52bits
        ulong temp;
        bool tooBig;

        while(!tooBig && (*currentCharacter == '_' || *currentCharacter == '0' || *currentCharacter == '1')) {
            if(*currentCharacter != '_') {
                temp <<= 1;
                temp |= *currentCharacter == '1';
                tooBig |= temp > MaxNumber;
            }

            currentCharacter++;
            this.currentLocation.lineOffset++;
        }

        if(tooBig) {
            // TODO: error
            return;
        }

        token.number = cast(double)temp;
        token.type = Token.Type.Number;
    }

    void lexOctalNumber(ref Token token) @trusted {
        enum MaxNumber = 0xFFFFFFFFFFFFF;

        // max 52bits
        ulong temp;
        bool tooBig;

        while(!tooBig && (*currentCharacter == '_' || *currentCharacter >= '0' && *currentCharacter <= '7')) {
            if(*currentCharacter != '_') {
                temp *= 8;
                temp += (*currentCharacter) - '0';
                tooBig |= temp > MaxNumber;
            }

            currentCharacter++;
            this.currentLocation.lineOffset++;
        }

        if(tooBig) {
            // TODO: error
            return;
        }

        token.number = cast(double)temp;
        token.type = Token.Type.Number;
    }

    void lexHexNumber(ref Token token) @trusted {
        enum MaxNumber = 0xFFFFFFFFFFFFF;

        // max 52bits
        ulong temp;
        bool tooBig;

        while(!tooBig && (*currentCharacter == '_' || (*currentCharacter >= '0' && *currentCharacter <= '7') ||
                (*currentCharacter >= 'a' && *currentCharacter <= 'f') || (*currentCharacter >= 'A' && *currentCharacter <= 'F'))) {
            if(*currentCharacter != '_') {
                temp <<= 4;

                if((*currentCharacter >= '0' && *currentCharacter <= '7'))
                    temp += (*currentCharacter) - '0';
                else if(*currentCharacter >= 'a' && *currentCharacter <= 'f')
                    temp += (*currentCharacter) - 'a';
                else if(*currentCharacter >= 'A' && *currentCharacter <= 'F')
                    temp += (*currentCharacter) - 'A';

                tooBig |= temp > MaxNumber;
            }

            currentCharacter++;
            this.currentLocation.lineOffset++;
        }

        if(tooBig) {
            // TODO: error
            return;
        }

        token.number = cast(double)temp;
        token.type = Token.Type.Number;
    }

    void lexDecimalNumber(ref Token token) @trusted {
        import core.stdc.math : pow;

        const(char)* startOfToken = currentCharacter;

        // max 52bits
        long whole, part;

        while(*currentCharacter == '_' || *currentCharacter >= '0' && *currentCharacter <= '9') {
            if(*currentCharacter != '_') {
                whole *= 10;
                whole += (*currentCharacter) - '0';
            }

            currentCharacter++;
            this.currentLocation.lineOffset++;
        }

        if(*currentCharacter == 'n') {
            // big integer

            const(char)[] slice = startOfToken[0 .. currentCharacter - startOfToken];

            token.type = Token.Type.BigInteger;
            token.bigInteger = DynamicBigInteger.parse(slice);

            currentCharacter++;
            this.currentLocation.lineOffset++;
            return;
        } else
            token.type = Token.Type.Number;

        if(*currentCharacter == '.') {
            // floating point
            currentCharacter++;
            this.currentLocation.lineOffset++;

            int digitsCount;

            while(*currentCharacter == '_' || *currentCharacter >= '0' && *currentCharacter <= '9') {
                if(*currentCharacter != '_') {
                    part *= 10;
                    part += (*currentCharacter) - '0';

                    digitsCount++;
                }

                currentCharacter++;
                this.currentLocation.lineOffset++;
            }

            if(digitsCount > 0)
                token.number = whole + (double(part) * pow(10, -digitsCount));
            else
                token.number = whole;
        } else
            token.number = whole;

        if(*currentCharacter == 'e' || *currentCharacter == 'E') {
            currentCharacter++;
            this.currentLocation.lineOffset++;

            int exponent;
            bool negativeExponent;

            if(*currentCharacter == '+') {
                currentCharacter++;
                this.currentLocation.lineOffset++;
            } else if(*currentCharacter == '-') {
                currentCharacter++;
                this.currentLocation.lineOffset++;
                negativeExponent = true;
            }

            while(*currentCharacter == '_' || *currentCharacter >= '0' && *currentCharacter <= '9') {
                if(*currentCharacter != '_') {
                    exponent *= 10;
                    exponent += (*currentCharacter) - '0';
                }

                currentCharacter++;
                this.currentLocation.lineOffset++;
            }

            if(negativeExponent)
                exponent *= -1;

            token.number *= pow(10, exponent);
        }
    }

    bool skipEscape(ref size_t consumed, ref size_t given) @trusted {
        import sidero.base.encoding.utf : encodeLengthUTF8, decode;

        // *(currentCharacter - 1) == '\\'

        switch(*currentCharacter) {
        case '\'':
        case '\\':
        case 'b':
        case 'f':
        case 'n':
        case 'r':
        case 't':
        case 'v':
        case '0':
            currentCharacter++;
            this.currentLocation.lineOffset++;
            consumed += 2;
            given++;
            break;

        case 'u':
            this.currentCharacter++;
            this.currentLocation.lineOffset++;
            consumed += 2;

            dchar temp = 0;

            if(*currentCharacter == '{') {
                // escape 6 is a code point
                this.currentCharacter++;
                this.currentLocation.lineOffset++;
                consumed++;

                static foreach(i; 0 .. 6) {
                    if(*currentCharacter == '}')
                        goto DoneEscape6;

                    static if(i > 0)
                        temp <<= 4;

                    if(*currentCharacter >= 'a' && *currentCharacter <= 'f')
                        temp |= ((*currentCharacter) - 'a') + 10;
                    else if(*currentCharacter >= 'A' && *currentCharacter <= 'F')
                        temp |= ((*currentCharacter) - 'A') + 10;
                    else if(*currentCharacter >= '0' && *currentCharacter <= '9')
                        temp |= (*currentCharacter) - '0';
                    else {
                        goto NotAnEscapeError;
                    }

                    this.currentCharacter++;
                    this.currentLocation.lineOffset++;
                    consumed++;
                }
        DoneEscape6:

                if(*currentCharacter != '}')
                    goto NotAnEscapeError;

                consumed++;
                given += encodeLengthUTF8(temp);

                this.currentCharacter++;
                this.currentLocation.lineOffset++;
                break;
            } else {
                // escape 4 is a UTF-16 code unit
                // at this level we don't need to care about surrogates

                static foreach(i; 0 .. 4) {
                    static if(i > 0)
                        temp <<= 4;

                    if(*currentCharacter >= 'a' && *currentCharacter <= 'f')
                        temp |= ((*currentCharacter) - 'a') + 10;
                    else if(*currentCharacter >= 'A' && *currentCharacter <= 'F')
                        temp |= ((*currentCharacter) - 'A') + 10;
                    else if(*currentCharacter >= '0' && *currentCharacter <= '9')
                        temp |= (*currentCharacter) - '0';
                    else
                        goto NotAnEscapeError;

                    currentCharacter++;
                }

                this.currentLocation.lineOffset += 4;
                consumed += 4;
                given++;

                if(temp >= 0xD800 && temp <= 0xDBFF) {
                    // this is a UTF-16 surrogate

                    wchar[2] temp2 = [cast(wchar)temp, 0];
                    char c;

                    if(!(*currentCharacter == '\\' && *(currentCharacter + 1) == 'u'))
                        goto EscapeSurrogateError;
                    else if(*(currentCharacter + 2) == '{')
                        goto EscapeSurrogateError;

                    consumed += 2;

                    static foreach(i; 0 .. 4) {
                        static if(i > 0)
                            temp2[1] <<= 4;

                        c = *(currentCharacter + i + 2);

                        if(c >= 'a' && c <= 'z')
                            temp2[1] |= (c - 'a') + 10;
                        else if(c >= 'A' && c <= 'Z')
                            temp2[1] |= (c - 'A') + 10;
                        else if(c >= '0' && c <= '9')
                            temp2[1] |= c - '0';
                        else
                            assert(0, "Handle identifier escape 4 ICE");
                    }

                    currentCharacter += 6;
                    consumed += 4;

                    if(temp2[1] < 0xDC00 || temp2[1] > 0xDFFF)
                        goto EscapeSurrogateError;

                    decode(temp2, temp);
                    given += encodeLengthUTF8(temp);
                }
                break;
            }

        default:
            goto NotAnEscapeError;
        }

        return true;

    EscapeSurrogateError:
        return false;

    NotAnEscapeError:
        return false;
    }

    dchar parseEscape(ref const(char)* ptr, char[] toFill, ref size_t used) @trusted {
        import sidero.base.encoding.utf : encodeUTF8, decode;

        // *(ptr - 1) == '\\'

        switch(*ptr) {
        case '\'':
            ptr++;
            if(toFill !is null)
                toFill[used++] = '\'';
            return '\'';
        case '\\':
            ptr++;
            if(toFill !is null)
                toFill[used++] = '\\';
            return '\\';

        case 'b':
            ptr++;
            if(toFill !is null)
                toFill[used++] = '\b';
            return '\b';

        case 'f':
            ptr++;
            if(toFill !is null)
                toFill[used++] = '\f';
            return '\f';

        case 'n':
            ptr++;
            if(toFill !is null)
                toFill[used++] = '\n';
            return '\n';

        case 'r':
            ptr++;
            if(toFill !is null)
                toFill[used++] = '\r';
            return '\r';

        case 't':
            ptr++;
            if(toFill !is null)
                toFill[used++] = '\t';
            return '\t';

        case 'v':
            ptr++;
            if(toFill !is null)
                toFill[used++] = '\v';
            return '\v';

        case '0':
            ptr++;
            if(toFill !is null)
                toFill[used++] = '\0';
            return '\0';

        case 'u':
            dchar temp = 0;
            ptr++;

            if(*ptr == '{') {
                // escape 6 is a code point
                ptr++;

                static foreach(i; 0 .. 6) {
                    if(*ptr == '}')
                        goto DoneEscape6;

                    static if(i > 0)
                        temp <<= 4;

                    if(*ptr >= 'a' && *ptr <= 'f')
                        temp |= ((*ptr) - 'a') + 10;
                    else if(*ptr >= 'A' && *ptr <= 'F')
                        temp |= ((*ptr) - 'A') + 10;
                    else if(*ptr >= '0' && *ptr <= '9')
                        temp |= (*ptr) - '0';
                    else
                        assert(0);

                    ptr++;
                }
        DoneEscape6:

                if(*ptr != '}')
                    assert(0);

                if(toFill !is null)
                    used += encodeUTF8(temp, *cast(char[4]*)toFill[used .. $].ptr);

                ptr++;
                return temp;
            } else {
                // escape 4 is a UTF-16 code unit
                // at this level we don't need to care about surrogates

                static foreach(i; 0 .. 4) {
                    static if(i > 0)
                        temp <<= 4;

                    if(*ptr >= 'a' && *ptr <= 'f')
                        temp |= ((*ptr) - 'a') + 10;
                    else if(*ptr >= 'A' && *ptr <= 'F')
                        temp |= ((*ptr) - 'A') + 10;
                    else if(*ptr >= '0' && *ptr <= '9')
                        temp |= (*ptr) - '0';
                    else
                        assert(0);

                    ptr++;
                }

                if(temp >= 0xD800 && temp <= 0xDBFF) {
                    // this is a UTF-16 surrogate

                    wchar[2] temp2 = [cast(wchar)temp, 0];
                    char c;

                    if(!(*ptr == '\\' && *(ptr + 1) == 'u'))
                        assert(0);
                    else if(*(ptr + 2) == '{')
                        assert(0);

                    static foreach(i; 0 .. 4) {
                        static if(i > 0)
                            temp2[1] <<= 4;

                        c = *(ptr + i + 2);

                        if(c >= 'a' && c <= 'z')
                            temp2[1] |= (c - 'a') + 10;
                        else if(c >= 'A' && c <= 'Z')
                            temp2[1] |= (c - 'A') + 10;
                        else if(c >= '0' && c <= '9')
                            temp2[1] |= c - '0';
                        else
                            assert(0, "Handle identifier escape 4 ICE");
                    }

                    ptr += 6;

                    if(temp2[1] < 0xDC00 || temp2[1] > 0xDFFF)
                        assert(0);

                    decode(temp2, temp);
                }

                if(toFill !is null)
                    used += encodeUTF8(temp, *cast(char[4]*)toFill[used .. $].ptr);

                return temp;
            }

        default:
            assert(0);
        }
    }
}

unittest {
    alias Token = Lexer_Javascript.Token;
    import sidero.base.console;
    import std.conv : text;

    struct Check {
    static:
        Token testSuccess2(string filename, string contents, Token.Type expectedType, bool allowNext = false) {
            Lexer_Javascript lexer = Lexer_Javascript(String_UTF8(filename), String_UTF8(contents));

            auto got = lexer.token;

            if(got.type != expectedType) {
                debugWriteln(got);
                assert(0);
            }

            lexer.popFront();

            if(!allowNext && lexer.front != Token.Type.EndOfFile) {
                debugWriteln(lexer.token);
                assert(0);
            }

            return got;
        }

        Token testSuccess(string mod = __MODULE__, int line = __LINE__)(string contents, Token.Type expectedType, bool allowNext = false) {
            return testSuccess2(text(mod, ":", line), contents, expectedType, allowNext);
        }

        void testSuccessIdentifier(string mod = __MODULE__, int line = __LINE__)(string contents, string expected) {
            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.Identifier);

            if(token.text != expected) {
                debugWriteln(token);
                assert(0);
            }
        }

        void testSuccessNumber(string mod = __MODULE__, int line = __LINE__)(string contents, double expected) {
            import sidero.base.math.utils;

            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.Number);

            if(!isClose(token.number, expected)) {
                debugWriteln(token, token.number);
                assert(0);
            }
        }

        void testSuccessBigInteger(string mod = __MODULE__, int line = __LINE__)(string contents, DynamicBigInteger expected) {
            import sidero.base.math.utils;

            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.BigInteger);

            if(token.bigInteger != expected) {
                debugWriteln(token, expected, token.bigInteger.haveDigits, expected.haveDigits,
                        token.bigInteger.storage, expected.storage);
                assert(0);
            }
        }

        void testSuccessPunctuation(string mod = __MODULE__, int line = __LINE__)(string contents, char expected, bool allowNext = false) {
            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.Punctuation, allowNext);

            if(token.punctuation != expected) {
                debugWriteln(token);
                assert(0);
            }
        }

        void testSuccessHashBangComment(string mod = __MODULE__, int line = __LINE__)(string contents, string expected) {
            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.HashBangComment);

            if(token.text != expected) {
                debugWriteln(token);
                assert(0);
            }
        }

        void testSuccessSingleLineComment(string mod = __MODULE__, int line = __LINE__)(string contents, string expected) {
            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.SingleLineComment);

            if(token.text != expected) {
                debugWriteln(token);
                assert(0);
            }
        }

        void testSuccessMultiLineComment(string mod = __MODULE__, int line = __LINE__)(string contents, string expected) {
            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.MultiLineComment);

            if(token.text != expected) {
                debugWriteln(token);
                assert(0);
            }
        }

        void testSuccessCharacter(string mod = __MODULE__, int line = __LINE__)(string contents, dchar expected) {
            import sidero.base.math.utils;

            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.Character);

            if(token.character != expected) {
                debugWriteln(token, token.character);
                assert(0);
            }
        }

        void testSuccessString(string mod = __MODULE__, int line = __LINE__)(string contents, string expected) {
            import sidero.base.math.utils;

            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.String);

            if(token.text != expected) {
                debugWriteln(token);
                assert(0);
            }
        }
    }

    Check.testSuccessIdentifier("$h_el9lo5", "$h_el9lo5");
    Check.testSuccessIdentifier("$b\\u0053e", "$bSe");
    Check.testSuccessIdentifier("$b\\u{000054}e", "$bTe");
    Check.testSuccessIdentifier("$b\\uD835\\uDD04e", "$b\U0001D504e");

    Check.testSuccessIdentifier("\\u0053e", "Se");
    Check.testSuccessIdentifier("\\u{000054}e", "Te");
    Check.testSuccessIdentifier("\\uD835\\uDD04e", "\U0001D504e");

    //bin
    Check.testSuccessNumber("0b0", 0);
    Check.testSuccessNumber("0b1", 1);
    Check.testSuccessNumber("0b00", 0);
    Check.testSuccessNumber("0b01", 1);
    Check.testSuccessNumber("0b11", 3);
    Check.testSuccessNumber("0b1_1", 3);

    //oct
    Check.testSuccessNumber("0o0", 0);
    Check.testSuccessNumber("0o1", 1);
    Check.testSuccessNumber("0o00", 0);
    Check.testSuccessNumber("0o01", 1);
    Check.testSuccessNumber("0o11", 9);
    Check.testSuccessNumber("0o1_1", 9);

    //hex
    Check.testSuccessNumber("0x0", 0);
    Check.testSuccessNumber("0x1", 1);
    Check.testSuccessNumber("0x00", 0);
    Check.testSuccessNumber("0x01", 1);
    Check.testSuccessNumber("0x11", 0x11);
    Check.testSuccessNumber("0x1_1", 0x11);

    //dec
    Check.testSuccessNumber("0", 0);
    Check.testSuccessNumber("00", 0);
    Check.testSuccessNumber("01", 1);
    Check.testSuccessNumber("00", 0);
    Check.testSuccessNumber("01", 1);
    Check.testSuccessNumber("011", 11);
    Check.testSuccessNumber("01_1", 11);

    // dec > 0
    Check.testSuccessNumber("1", 1);
    Check.testSuccessNumber("11", 11);
    Check.testSuccessNumber("1_1", 11);

    //n
    Check.testSuccessBigInteger("0n", DynamicBigInteger.parse("0"));
    Check.testSuccessBigInteger("00n", DynamicBigInteger.parse("0"));
    Check.testSuccessBigInteger("01n", DynamicBigInteger.parse("1"));
    Check.testSuccessBigInteger("00n", DynamicBigInteger.parse("0"));
    Check.testSuccessBigInteger("01n", DynamicBigInteger.parse("1"));
    Check.testSuccessBigInteger("011n", DynamicBigInteger.parse("11"));
    Check.testSuccessBigInteger("01_1n", DynamicBigInteger.parse("11"));

    //.
    Check.testSuccessNumber("0.0", 0);
    Check.testSuccessNumber("0.1", 0.1);
    Check.testSuccessNumber("0.0", 0);
    Check.testSuccessNumber("0.11", 0.11);
    Check.testSuccessNumber("0.1_1", 0.11);

    // . punctuation
    Check.testSuccessPunctuation(".", '.');
    Check.testSuccessPunctuation(". a", '.', true);
    Check.testSuccessPunctuation(".a", '.', true);
    Check.testSuccessPunctuation(". 9", '.', true);

    //. num
    Check.testSuccessNumber(".0", 0);
    Check.testSuccessNumber(".1", 0.1);
    Check.testSuccessNumber(".11", 0.11);
    Check.testSuccessNumber(".1_1", 0.11);

    //.
    //0 exp
    Check.testSuccessNumber("0.0e0", 0);
    Check.testSuccessNumber("0.1e0", 0.1);
    Check.testSuccessNumber("0.0e0", 0);
    Check.testSuccessNumber("0.11e0", 0.11);
    Check.testSuccessNumber("0.1_1e0", 0.11);

    //.
    //+exp
    Check.testSuccessNumber("0.0e1", 0);
    Check.testSuccessNumber("0.1e1", 1);
    Check.testSuccessNumber("0.1e+1", 1);
    Check.testSuccessNumber("0.0e1", 0);
    Check.testSuccessNumber("0.11e1", 1.1);
    Check.testSuccessNumber("0.1_1e1", 1.1);

    Check.testSuccessNumber("0.0e2", 0);
    Check.testSuccessNumber("0.1e2", 10);
    Check.testSuccessNumber("0.0e2", 0);
    Check.testSuccessNumber("0.11e2", 11);
    Check.testSuccessNumber("0.1_1e2", 11);

    //.
    //-exp
    Check.testSuccessNumber("0.0e-1", 0);
    Check.testSuccessNumber("0.1e-1", 0.01);
    Check.testSuccessNumber("0.1e-1", 0.01);
    Check.testSuccessNumber("0.0e-1", 0);
    Check.testSuccessNumber("0.11e-1", 0.011);
    Check.testSuccessNumber("0.1_1e-1", 0.011);

    Check.testSuccessNumber("0.0e-2", 0);
    Check.testSuccessNumber("0.1e-2", 0.001);
    Check.testSuccessNumber("0.0e-2", 0);
    Check.testSuccessNumber("0.11e-2", 0.0011);
    Check.testSuccessNumber("0.1_1e-2", 0.0011);

    // hash bang comment
    Check.testSuccessHashBangComment("#!some text", "some text");
    Check.testSuccessHashBangComment("#! some text", "some text");
    Check.testSuccessHashBangComment("#! some text\r", "some text\r");
    Check.testSuccessHashBangComment("#! some text\r\n", "some text");
    Check.testSuccessHashBangComment("#! some text\n", "some text");

    // single line comment
    Check.testSuccessSingleLineComment("//some text", "some text");
    Check.testSuccessSingleLineComment("// some text", "some text");
    Check.testSuccessSingleLineComment("// some text\r", "some text\r");
    Check.testSuccessSingleLineComment("// some text\r\n", "some text");
    Check.testSuccessSingleLineComment("// some text\n", "some text");

    // / punctuation
    Check.testSuccessPunctuation("/", '/');
    Check.testSuccessPunctuation("/ a", '/', true);
    Check.testSuccessPunctuation("/a", '/', true);
    Check.testSuccessPunctuation("/ 9", '/', true);
    Check.testSuccessPunctuation("/ *", '/', true);
    Check.testSuccessPunctuation("/ /", '/', true);

    // multi line comment
    Check.testSuccessMultiLineComment("/*some text*/", "some text");
    Check.testSuccessMultiLineComment("/* some text*/", " some text");
    Check.testSuccessMultiLineComment("/* some text\r*/", " some text\r");
    Check.testSuccessMultiLineComment("/* some text\n*/", " some text\n");
    Check.testSuccessMultiLineComment("/* some text\r\n*/", " some text\r\n");
    Check.testSuccessMultiLineComment("/* some text*/\r\n", " some text");
    Check.testSuccessMultiLineComment("/* some text*/\n", " some text");

    // characters
    Check.testSuccessCharacter("'a'", 'a');
    Check.testSuccessCharacter("'\n'", '\n');
    Check.testSuccessCharacter("'\\n'", '\n');
    Check.testSuccessCharacter("'\\b'", '\b');
    Check.testSuccessCharacter("'\\u0053'", 'S');
    Check.testSuccessCharacter("'\\u{000054}'", 'T');
    Check.testSuccessCharacter("'\\uD835\\uDD04'", '\U0001D504');

    // strings
    Check.testSuccessString("\"bae\"", "bae");
    Check.testSuccessString("\"b\ne\"", "b\ne");
    Check.testSuccessString("\"b\\ne\"", "b\ne");
    Check.testSuccessString("\"b\\be\"", "b\be");
    Check.testSuccessString("\"b\\u0053e\"", "bSe");
    Check.testSuccessString("\"b\\u{000054}e\"", "bTe");
    Check.testSuccessString("\"b\\uD835\\uDD04e\"", "b\U0001D504e");
}
