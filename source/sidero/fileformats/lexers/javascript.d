module sidero.fileformats.lexers.javascript;
import sidero.fileformats.lexers.interning;
import sidero.fileformats.lexers.defs;
import sidero.fileformats.errors;
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
        supports U+2028 and U+2029, \r\n \n
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

- regular expression
     / start next? / flags
     flags: (Continue | $)*
     start:
        [ ClassChar* ]
        BackSlashSeq
        not-line-terminator but not * \ / [
     next:
        [ ClassChar* ]
        BackSlashSeq
        not-line-terminator but not \ / [
     ClassChar:
        BackSlashSeq
        not-line-terminator
    BackSlashSeq: \ not-line-terminator
+/

/**
For identifiers is UAX31 compliant.

For numbers, negation is not handled due to not having the ability to determine between unary and binary.

Regex literals must be enabled by passing a boolean flag to the front/token/peek/peek2 methods.
*/
struct Lexer_Javascript {
    private {
        import sidero.fileformats.internal.smallarray;

        Token[3] tokens;
        const(char)*[3] fileOffsetsGivenToken;

        Lexer_Interning interning;
        String_UTF8 contentsStorage;

        ErrorSinkRef errorSink;

        Loc currentLocation;

        const(char)* startOfFile, currentCharacter, endOfFile;
        size_t haveTokens;

        bool hitError;
        bool inTemplate, inTemplateSubstitution;
    }

    ///
    static struct Token {
        ///
        Type type;

        ///
        Loc loc;

        ///
        union {
            ///
            String_UTF8 text;

            ///
            DynamicBigInteger bigInteger;

            ///
            double number;

            ///
            wchar punctuation;

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
            String,
            ///
            RegexString,
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

        this(return scope ref Token other) scope @trusted {
            this.type = other.type;
            this.loc = other.loc;

            final switch(other.type) {
            case Type.EndOfFile:
                return;

            case Type.Punctuation:
                this.punctuation = other.punctuation;
                return;

            case Type.HashBangComment:
            case Type.SingleLineComment:
            case Type.MultiLineComment:
            case Type.Identifier:
            case Type.String:
            case Type.RegexString:
            case Type.TemplateSubstitutionHead:
            case Type.TemplateSubstitutionMiddle:
            case Type.TemplateSubstitutionTail:
                this.text = other.text;
                return;

            case Type.Number:
                this.number = other.number;
                return;

            case Type.BigInteger:
                this.bigInteger = other.bigInteger;
                return;
            }
        }

        ~this() scope @trusted {
            final switch(this.type) {
            case Type.EndOfFile:
            case Type.Punctuation:
            case Type.Number:
                return;

            case Type.HashBangComment:
            case Type.SingleLineComment:
            case Type.MultiLineComment:
            case Type.Identifier:
            case Type.String:
            case Type.RegexString:
            case Type.TemplateSubstitutionHead:
            case Type.TemplateSubstitutionMiddle:
            case Type.TemplateSubstitutionTail:
                this.text.destroy;
                return;

            case Type.BigInteger:
                this.bigInteger.destroy;
                return;
            }
        }

        void opAssign(return scope Token other) scope {
            this.destroy;
            this.__ctor(other);
        }
    }

    ///
    enum PunctuationChars : wchar {
        ///
        OpenBrace = '{',
        ///
        CloseBrace = '}',
        ///
        OpenBracket = '(',
        ///
        CloseBracket = ')',
        ///
        OpenSquareBracket = '[',
        ///
        CloseSquareBracket = ']',
        ///
        Divide = '/',
        ///
        Dot = '.',
        ///
        Colon = ':',
        ///
        SemiColon = ';',
        ///
        Comma = ',',
        ///
        LessThan = '<',
        ///
        MoreThan = '>',
        ///
        Plus = '+',
        ///
        Minus = '-',
        ///
        Star = '*',
        ///
        Percent = '%',
        ///
        And = '&',
        ///
        Pipe = '|',
        ///
        Caret = '^',
        ///
        ExclamationMark = '!',
        ///
        Tilde = '~',
        ///
        QuestionMark = '?',
        ///
        EqualSign = '=',

        /// ...
        ListConcatenation = '\uE000',
        /// <=
        LessThanEqualTo,
        /// >=
        MoreThanEqualTo,
        /// ==
        EqualTo,
        /// !=
        NotEqualTo,
        /// ===
        StrictlyEqual,
        /// !==
        StrictlyNotEqual,
        /// **
        Exponention,
        /// ++
        Increment,
        /// --
        Decrement,
        /// <<
        LeftShift,
        /// >>
        SignedRightShift,
        /// >>>
        UnsignedRightShift,
        /// &&
        BooleanAnd,
        /// ||
        BooleanOr,

        /// ??
        Coalesce,
        /// ?.
        OptionalChain,
        /// +=
        AddAssign,
        /// -=
        SubtractAssign,
        /// *=
        MultiplyAssign,
        /// /=
        DivideAssign,
        /// %=
        ModulasAssign,
        /// **=
        ExponentionAssign,
        /// <<=
        LeftShiftAssign,
        /// >>=
        SignedRightShiftAssign,
        /// >>>=
        UnsignedRightShiftAssign,
        /// &=
        AndAssign,
        /// |=
        OrAssign,
        /// ^=
        CaretAssign,
        /// &=
        BooleanAndAssign,
        /// ||=
        BooleanOrAssign,
        /// ??=
        CoalesceAssign,
        /// =>
        ArrowFunction,

        ///
        Max
    }

    ///
    static string punctuationToText(wchar c) @safe nothrow @nogc pure {
        static immutable Table1 = [
            "...", "<=", ">=", "==", "!=", "===", "!==", "**", "++", "--", "<<", ">>", ">>>", "&&", "||", "??", "?.",
            "+=", "-=", "*=", "/=", "%=", "**=", "<<=", ">>=", ">>>=", "&=", "|=", "^=", "&&=", "||=", "??=", "=>"
        ];
        static immutable Table2 = () {
            string ret;

            foreach(c; 0 .. 128 + 1) {
                ret ~= cast(char)c;
            }

            return ret;
        }();

        if(c >= PunctuationChars.ListConcatenation && c <= PunctuationChars.Max)
            return Table1[c - '\uE000'];
        else if(c < 128)
            return Table2[c .. c + 1];
        else
            return null;
    }

export @safe nothrow @nogc:

    ///
    this(String_UTF8 fileName, String_UTF8 contents, ErrorSinkRef errorSink) scope @trusted {
        if(!contents.isPtrNullTerminated)
            contents = contents.dup;

        this.currentLocation = Loc(fileName, 1, 1);
        this.contentsStorage = contents;

        this.errorSink = errorSink;
        assert(!this.errorSink.isNull);

        auto text = contents.unsafeGetLiteral();
        if(text !is null) {
            startOfFile = text.ptr;
            currentCharacter = startOfFile;
            endOfFile = text.ptr + text.length;
        }

        static String_UTF8[] tableOfKeywords = () {
            String_UTF8[] ret;

            static foreach(k; __traits(allMembers, Keywords)) {
                ret ~= __traits(getMember, Keywords, k);
            }

            return ret;
        }();

        this.interning.preFill(tableOfKeywords);
        this.popFront;
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

        // contextual
        nan = String_UTF8("NaN"), ///
        infinity = String_UTF8("Infinity"), ///
    }

    ///
    bool isNull() scope const {
        return this.currentLocation.fileName.isNull;
    }

    ///
    bool empty() scope const {
        return this.haveTokens == 0 || this.tokens[0].type == Token.Type.EndOfFile;
    }

    ///
    Token.Type front(bool regexAllowed = false) scope {
        fillToken(0, regexAllowed);
        return tokens[0].type;
    }

    ///
    void popFront() scope @trusted {
        if(this.haveTokens > 1) {
            foreach(i, v; this.tokens) {
                if(i == 0)
                    continue;

                this.tokens[i - 1] = v;
                this.fileOffsetsGivenToken[i - 1] = this.fileOffsetsGivenToken[i];
            }

            this.haveTokens--;
        } else {
            if(this.haveTokens > 0)
                this.haveTokens--;
            fillToken(0, false);
        }
    }

    ///
    Token token(bool regexAllowed = false) return scope {
        fillToken(0, regexAllowed);
        return this.tokens[0];
    }

    ///
    Token.Type peek(bool regexAllowed = false) scope {
        fillToken(0);
        fillToken(1, regexAllowed);
        assert(this.haveTokens > 1);
        return this.tokens[1].type;
    }

    ///
    Token.Type peek2(bool regexAllowed = false) scope {
        fillToken(0);
        fillToken(1);
        fillToken(2, regexAllowed);
        assert(this.haveTokens > 2);
        return this.tokens[2].type;
    }

private:
    void fillToken(size_t tokenOffset, bool regexAllowed = false) scope @trusted {
        if(this.haveTokens > tokenOffset) {
            if(regexAllowed) {
                if(this.tokens[tokenOffset].type == Token.Type.Punctuation && this.tokens[tokenOffset].punctuation == '/') {
                    // need to rescan from this point on.
                    this.haveTokens = tokenOffset;

                    this.currentCharacter = this.fileOffsetsGivenToken[tokenOffset];
                    this.currentLocation = this.tokens[tokenOffset].loc;

                    scan(this.tokens[tokenOffset], this.fileOffsetsGivenToken[tokenOffset], true);
                    this.haveTokens++;
                }
            } else if(this.tokens[tokenOffset].type == Token.Type.RegexString) {
                // Swap the regex string token for punctuation /

                this.tokens[tokenOffset].type = Token.Type.Punctuation;
                this.tokens[tokenOffset].punctuation = '/';

                this.haveTokens = tokenOffset + 1;
                this.currentCharacter = this.fileOffsetsGivenToken[tokenOffset] + 1;
                this.currentLocation = this.tokens[tokenOffset].loc;
                this.currentLocation.lineOffset++;
            }

            return;
        }

        this.tokens[tokenOffset] = Token.init;

        if(tokenOffset == 0) {
            scan(this.tokens[0], this.fileOffsetsGivenToken[tokenOffset], regexAllowed);
            this.haveTokens = 1;
        } else {
            assert(this.haveTokens == tokenOffset);
            scan(this.tokens[tokenOffset], this.fileOffsetsGivenToken[tokenOffset], regexAllowed);
            this.haveTokens++;
        }
    }

    void scan(scope out Token token, scope ref const(char)* startOfTokenOffsetPtr, bool regexAllowed) scope @trusted {
        if(this.inTemplate) {
            // https://262.ecma-international.org/15.0/index.html#prod-TemplateSubstitutionTail

            if(this.inTemplateSubstitution) {
                if(*currentCharacter == '}') {
                    // } ends scanning for js tokens
                    this.inTemplateSubstitution = false;
                    currentCharacter++;
                    this.currentLocation.lineOffset++;
                } else
                    return scanActual(token, startOfTokenOffsetPtr, regexAllowed);
            }

            // identical to head except its TemplateSubstitutionMiddle if ends with ${
            // otherwise its TemplateSubstitutionTail if it ends with `

            startOfTokenOffsetPtr = currentCharacter;
            parseTemplateLiteralBegin(token, false);
        } else
            scanActual(token, startOfTokenOffsetPtr, regexAllowed);
    }

    void scanActual(scope ref Token token, scope ref const(char)* startOfTokenOffsetPtr, bool regexAllowed) scope @trusted {
        import sidero.base.encoding.utf : decode, decodeLength;
        import sidero.base.text.unicode.characters.database;

        enum MaxNumber = 0xFFFFFFFFFFFFF;

        token.type = Token.Type.EndOfFile;

        if(this.hitError)
            return;

        while(currentCharacter < endOfFile) {
            startOfTokenOffsetPtr = currentCharacter;

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

                //"=>"

            case '(':
            case ')':
            case ',':
            case ':':
            case ';':
            case '[':
            case ']':
            case '{':
            case '}':
            case '~':
                token.type = Token.Type.Punctuation;
                token.punctuation = asciiChar;
                return;

            case '<': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '=') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        token.punctuation = PunctuationChars.LessThanEqualTo;
                    } else if(*currentCharacter == '<') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.LeftShiftAssign;
                        } else
                            token.punctuation = PunctuationChars.LeftShift;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

            case '>': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '=') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        token.punctuation = PunctuationChars.MoreThanEqualTo;
                    } else if(*currentCharacter == '>') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.SignedRightShiftAssign;
                        } else if(*currentCharacter == '>') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;

                            if(*currentCharacter == '=') {
                                currentCharacter++;
                                this.currentLocation.lineOffset++;
                                token.punctuation = PunctuationChars.UnsignedRightShiftAssign;
                            } else
                                token.punctuation = PunctuationChars.UnsignedRightShift;
                        } else
                            token.punctuation = PunctuationChars.SignedRightShift;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

            case '=': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '=') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.StrictlyEqual;
                        } else
                            token.punctuation = PunctuationChars.EqualTo;
                    } else if(*currentCharacter == '>') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        token.punctuation = PunctuationChars.ArrowFunction;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

            case '!': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '=') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.StrictlyNotEqual;
                        } else
                            token.punctuation = PunctuationChars.NotEqualTo;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

            case '*': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '*') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.ExponentionAssign;
                        } else
                            token.punctuation = PunctuationChars.Exponention;
                    } else if(*currentCharacter == '=') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        token.punctuation = PunctuationChars.MultiplyAssign;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

            case '&': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '&') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.BooleanAndAssign;
                        } else
                            token.punctuation = PunctuationChars.BooleanAnd;
                    } else if(*currentCharacter == '=') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        token.punctuation = PunctuationChars.AndAssign;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

            case '|': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '|') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.BooleanOrAssign;
                        } else
                            token.punctuation = PunctuationChars.BooleanOr;
                    } else if(*currentCharacter == '=') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        token.punctuation = PunctuationChars.OrAssign;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

            case '?': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '?') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;

                        if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.CoalesceAssign;
                        } else
                            token.punctuation = PunctuationChars.Coalesce;
                    } else if(*currentCharacter == '.') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        token.punctuation = PunctuationChars.OptionalChain;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

            case '%': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '=') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        token.punctuation = PunctuationChars.ModulasAssign;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

            case '^': {
                    token.type = Token.Type.Punctuation;

                    if(*currentCharacter == '=') {
                        currentCharacter++;
                        this.currentLocation.lineOffset++;
                        token.punctuation = PunctuationChars.CaretAssign;
                    } else
                        token.punctuation = asciiChar;

                    return;
                }

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
                                    goto EscapeIdentifierError;
                                else if(*(currentCharacter + 2) == '{')
                                    goto EscapeIdentifierError;

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
                    this.errorSink.error(token.loc, "Identifier start is not valid.");
                    this.errorSink.errorSupplimental("Valid characters must be part of start set.");
                    this.errorSink.errorSupplimental(
                            "May be an escaped UTF-16 code unit `\\uXXXX`, two may be used for UTF-16 surrogates.");
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

                        if(*currentCharacter == '.' && *(currentCharacter + 1) == '.') {
                            currentCharacter += 2;
                            this.currentLocation.lineOffset += 2;
                            token.punctuation = PunctuationChars.ListConcatenation;
                        } else
                            token.punctuation = asciiChar;
                    }

                    return;
                }

            case '-': {
                    if(*currentCharacter >= '0' && *currentCharacter <= '9') {
                        lexDecimalNumber(token, true);
                    } else {
                        token.type = Token.Type.Punctuation;

                        if(*currentCharacter == '-') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.Decrement;
                        } else if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.SubtractAssign;
                        } else
                            token.punctuation = asciiChar;
                    }

                    return;
                }

            case '+': {
                    if(*currentCharacter >= '0' && *currentCharacter <= '9') {
                        lexDecimalNumber(token);
                    } else {
                        token.type = Token.Type.Punctuation;

                        if(*currentCharacter == '+') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.Increment;
                        } else if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.AddAssign;
                        } else
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

                    errorSink.error(token.loc, "Unknown character `#`");
                    errorSink.errorSupplimental("Hash bang comments are of syntax `#!...\\n`");
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

                    if(regexAllowed) {
                        /*
                        - regular expression
                             / start next? / flags
                             flags: (Continue | $)*

                             next:
                                [ ClassChar* ]
                                BackSlashSeq
                                not-line-terminator but not \ / [
                             ClassChar:
                                BackSlashSeq
                                not-line-terminator
                            BackSlashSeq: \ not-line-terminator
                        */
                        const(char)* startToken = currentCharacter - 1;

                        {
                            // start

                            if(*currentCharacter == '\\') {
                                // BackSlashSeq: \ not-line-terminator

                                this.currentCharacter++;
                                this.currentLocation.lineOffset++;

                                if(isNewLineNext(this.currentCharacter)[0] > 0)
                                    goto RegexStartError;

                                const countChars = decodeLength(*currentCharacter);
                                this.currentCharacter += countChars;
                                this.currentLocation.lineOffset++;
                            } else if(*currentCharacter == '[') {
                                // [ ClassChar* ]
                                // ClassChar:
                                //     BackSlashSeq
                                //     not-line-terminator
                                // BackSlashSeq: \ not-line-terminator

                                this.currentCharacter++;
                                this.currentLocation.lineOffset++;

                                while(currentCharacter < endOfFile) {
                                    if(*currentCharacter == ']')
                                        break;

                                    if(*currentCharacter == '\\') {
                                        this.currentCharacter++;
                                        this.currentLocation.lineOffset++;
                                    }

                                    if(isNewLineNext(this.currentCharacter)[0] > 0)
                                        goto RegexStartError;

                                    const countChars = decodeLength(*currentCharacter);
                                    this.currentCharacter += countChars;
                                    this.currentLocation.lineOffset++;
                                }

                                if(*currentCharacter != ']')
                                    goto RegexStartError;

                                this.currentCharacter++;
                                this.currentLocation.lineOffset++;
                            } else {
                                // not-line-terminator but not * \ / [
                                if(isNewLineNext(this.currentCharacter)[0] > 0)
                                    goto RegexStartError;

                                const countChars = decodeLength(*currentCharacter);
                                this.currentCharacter += countChars;
                                this.currentLocation.lineOffset++;
                            }
                        }

                        {
                            /*
                            next:
                                [ ClassChar* ]
                                BackSlashSeq
                                not-line-terminator but not \ / [
                            */

                            while(currentCharacter < endOfFile) {
                                if(*currentCharacter == '/')
                                    break;
                                else if(*currentCharacter == '[') {
                                    // [ ClassChar* ]
                                    // ClassChar:
                                    //     BackSlashSeq
                                    //     not-line-terminator
                                    // BackSlashSeq: \ not-line-terminator

                                    this.currentCharacter++;
                                    this.currentLocation.lineOffset++;

                                    while(currentCharacter < endOfFile) {
                                        if(*currentCharacter == ']')
                                            break;

                                        if(*currentCharacter == '\\') {
                                            this.currentCharacter++;
                                            this.currentLocation.lineOffset++;
                                        }

                                        if(isNewLineNext(this.currentCharacter)[0] > 0)
                                            goto RegexNextError;

                                        const countChars = decodeLength(*currentCharacter);
                                        this.currentCharacter += countChars;
                                        this.currentLocation.lineOffset++;
                                    }

                                    if(*currentCharacter != ']')
                                        goto RegexNextError;

                                    this.currentCharacter++;
                                    this.currentLocation.lineOffset++;
                                } else {
                                    // not-line-terminator but not \ / [
                                    // BackSlashSeq: \ not-line-terminator

                                    if(*currentCharacter == '\\') {
                                        this.currentCharacter++;
                                        this.currentLocation.lineOffset++;
                                    }

                                    if(isNewLineNext(this.currentCharacter)[0] > 0)
                                        goto RegexNextError;

                                    const countChars = decodeLength(*currentCharacter);
                                    this.currentCharacter += countChars;
                                    this.currentLocation.lineOffset++;
                                }
                            }
                        }

                        {
                            if(*currentCharacter != '/')
                                goto RegexEndError;

                            this.currentCharacter++;
                            this.currentLocation.lineOffset++;

                            // flags: (Continue | $)*
                            while(currentCharacter < endOfFile) {
                                if(*currentCharacter == '$') {
                                    this.currentCharacter++;
                                    this.currentLocation.lineOffset++;
                                } else {
                                    asciiChar = *currentCharacter;

                                    if((asciiChar >= 'a' && asciiChar <= 'z') || (asciiChar >= 'A' && asciiChar <= 'Z') ||
                                            (asciiChar >= '0' && asciiChar <= '9') || asciiChar == '_') {
                                        this.currentCharacter++;
                                        this.currentLocation.lineOffset++;
                                        continue;
                                    } else {
                                        size_t consumed, offset;
                                        dchar decoded = decode(() @trusted {
                                            return currentCharacter + offset >= (endOfFile + 1);
                                        }, () @trusted { return cast(char)*(currentCharacter + offset); }, () {
                                            offset++;
                                        }, consumed);

                                        if(isUAX31_Javascript_Continue(decoded)) {
                                            this.currentCharacter += consumed;
                                            this.currentLocation.lineOffset++;
                                            continue;
                                        } else
                                            break;
                                    }
                                }
                            }

                            token.type = Token.Type.RegexString;
                            token.text = String_UTF8(startToken[0 .. currentCharacter - startToken]).dup;
                        }
                    } else {
                        // punctuation
                        token.type = Token.Type.Punctuation;

                        if(*currentCharacter == '=') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;
                            token.punctuation = PunctuationChars.DivideAssign;
                        } else
                            token.punctuation = asciiChar;
                    }

                    return;

            RegexStartError:
                    errorSink.error(token.loc, "Not a valid start for regex literal");
                    errorSink.errorSupplimental(
                            "Must be either a class `/[...]`, escape `\\c`, or a character that isn't `/` or a new line.");
                    return;

            RegexNextError:
                    errorSink.error(token.loc, "Not a valid body for regex literal");
                    errorSink.errorSupplimental("Must be either a class `/[...]`, escape `\\c`, or a character that isn't a new line.");
                    return;

            RegexEndError:
                    errorSink.error(token.loc, "Not a valid end for regex literal, missing `/`");
                    return;
                }

            case '\'':
            case '\"': {
                    char endOfTokenCharacter = asciiChar;
                    const(char)* startToken = currentCharacter;
                    size_t consumed, given;

                    while(currentCharacter < endOfFile) {
                        asciiChar = *currentCharacter;

                        if(asciiChar == '\\') {
                            currentCharacter++;
                            this.currentLocation.lineOffset++;

                            if(!skipEscape(consumed, given))
                                goto StringLiteralEscapeError;
                        } else if(asciiChar == endOfTokenCharacter) {
                            break;
                        } else {
                            const countChars = decodeLength(asciiChar);

                            if(isNewLineNext(this.currentCharacter)[0] > 0) {
                                this.currentLocation.lineNumber++;
                                this.currentLocation.lineOffset = 1;
                            } else
                                this.currentLocation.lineOffset++;

                            currentCharacter += countChars;
                        }
                    }

                    if(*currentCharacter != endOfTokenCharacter)
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
                            } else if(asciiChar == endOfTokenCharacter) {
                                break;
                            } else {
                                if(isNewLineNext(ptr)[0] > 0) {
                                    this.currentLocation.lineNumber++;
                                    this.currentLocation.lineOffset = 1;
                                } else
                                    this.currentLocation.lineOffset++;

                                const countChars = decodeLength(asciiChar);

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
                    errorSink.error(token.loc, "invalid string literal `\"str\"` or `'str'`");
                    errorSink.errorSupplimental("May be escaped `\\uXXXX` must be a UTF-16 code unit, two UTF-16 surrogates are supported");
                    return;
                }

            case '`':
                parseTemplateLiteralBegin(token, true);
                return;

            default: {
                    currentCharacter--;
                    this.currentLocation.lineOffset--;

                    if(*currentCharacter == 0) {
                        return;
                    } else if(isNewLineNext(this.currentCharacter)[0] > 0) {
                        this.currentLocation.lineNumber++;
                        this.currentLocation.lineOffset = 1;
                        continue;
                    } else {
                        size_t consumed, offset;
                        dchar decoded = decode(() @trusted { return currentCharacter + offset >= endOfFile; }, () @trusted {
                            return cast(char)*(currentCharacter + offset);
                        }, () { offset++; }, consumed);

                        if(decoded == '\uFEFF' || getGeneralCategory(decoded) == GeneralCategory.Zs) {
                            this.currentLocation.lineOffset++;
                            continue;
                        } else if(isUAX31_Javascript_Start(decoded)) {
                            this.currentCharacter += consumed;
                            this.currentLocation.lineOffset++;
                            lexIdentifier(token, currentCharacter - consumed, 0, 0, 0);
                            return;
                        }

                        errorSink.error(currentLocation, "invalid character `{:s}`(0x{:X})", decoded, cast(uint)decoded);
                        return;
                    }
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
                errorSink.error(token.loc, "invalid identifier");
                errorSink.errorSupplimental("Expecting an identifier with a valid start, then zero or more valid continue characters.");
                errorSink.errorSupplimental("May be escaped `\\uXXXX` which is a UTF-16 code unit, two may be used for surrogates.");
                errorSink.errorSupplimental("A code unit may be escaped as `\\u{XXXXXX}`.");
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
        errorSink.error(currentLocation, "invalid escaped UTF-16 surrogate in identifier");
        errorSink.errorSupplimental("May be escaped `\\uXXXX` which is a UTF-16 code unit, two may be used for surrogates.");
        errorSink.errorSupplimental("A code unit may be escaped as `\\u{XXXXXX}`.");
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
            errorSink.error(token.loc, "binary number is too big");
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
            errorSink.error(token.loc, "octal number is too big");
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
            errorSink.error(token.loc, "hex number is too big");
            return;
        }

        token.number = cast(double)temp;
        token.type = Token.Type.Number;
    }

    void lexDecimalNumber(ref Token token, bool negate = false) @trusted {
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

            if(negate)
                token.bigInteger = -token.bigInteger;
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

        if(negate)
            token.number *= -1;
    }

    void parseTemplateLiteralBegin(ref Token token, bool isHead) @trusted {
        import sidero.base.encoding.utf : decodeLength;

        // template substitution https://262.ecma-international.org/15.0/index.html#prod-TemplateSubstitutionTail

        const(char)* startToken = currentCharacter;
        size_t consumed, given;

        char asciiChar;

        while(currentCharacter < endOfFile) {
            asciiChar = *currentCharacter;

            if(asciiChar == '\\') {
                currentCharacter++;
                this.currentLocation.lineOffset++;

                if(!skipEscape(consumed, given))
                    goto TemplateLiteralEscapeError;
            } else if(asciiChar == '`' || (asciiChar == '$' && *(currentCharacter + 1) == '{')) {
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

        {
            SmallArray!char array = SmallArray!char(((currentCharacter - startToken) - consumed) + given);
            char[] buffer = array.get;
            size_t usedOfBuffer;

            const(char)* ptr = startToken;

            while(ptr < endOfFile) {
                asciiChar = *ptr;

                if(asciiChar == '\\') {
                    ptr++;
                    dchar decoded = parseEscape(ptr, buffer, usedOfBuffer);
                } else if(asciiChar == '`' || (asciiChar == '$' && *(ptr + 1) == '{')) {
                    break;
                } else {
                    const countChars = decodeLength(asciiChar);

                    foreach(i; 0 .. countChars) {
                        buffer[usedOfBuffer++] = *(ptr + i);
                    }

                    ptr += countChars;
                }
            }

            if(*currentCharacter == '`') {
                // stop at `
                currentCharacter++;
                this.currentCharacter++;
                token.type = isHead ? Token.Type.String : Token.Type.TemplateSubstitutionTail;

                this.inTemplate = false;
            } else if(*currentCharacter == '$' && *(currentCharacter + 1) == '{') {
                // stop at ${
                this.inTemplate = true;
                token.type = isHead ? Token.Type.TemplateSubstitutionHead : Token.Type.TemplateSubstitutionMiddle;

                this.currentCharacter += 2;
                this.currentLocation.lineOffset += 2;
                this.inTemplateSubstitution = true;
            } else
                goto TemplateLiteralError;

            token.text = String_UTF8(buffer[0 .. usedOfBuffer]).dup;
            return;
        }

    TemplateLiteralError:
        errorSink.error(currentLocation, "invalid template literal character `{:s}`(0x{:X})", *currentCharacter,
                cast(uint)*currentCharacter);
        return;

    TemplateLiteralEscapeError:
        errorSink.error(currentLocation, "invalid template literal escape");
        return;
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
            size_t[2] consumedGiven = isNewLineNext(this.currentCharacter);

            if(consumedGiven[0] > 0) {
                this.currentCharacter += consumedGiven[0];
                this.currentLocation.lineOffset = 1;
                this.currentLocation.lineNumber++;
                consumed += consumedGiven[0];
            } else
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
            size_t[2] consumedGiven = isNewLineNext(ptr);

            if(consumedGiven[0] > 0) {
                ptr += consumedGiven[0];
                return '\n';
            } else
                assert(0);
        }
    }

    static size_t[2] isNewLineNext(scope ref const(char)* ptr) @trusted {
        if(*ptr == '\n') {
            return [1, 1];
        } else if(*ptr == '\r' && *(ptr + 1) == '\n') {
            return [2, 2];
        } else if(*ptr == 0xE2 && *(ptr + 1) == 0x80 && (*(ptr + 2) == 0xA8 || *(ptr + 2) == 0xA9)) {
            return [3, 1];
        } else
            return [0, 0];
    }
}

unittest {
    alias Token = Lexer_Javascript.Token;
    import sidero.base.console;
    import std.conv : text;

    struct Check {
    static:
        Token testSuccess2(string filename, string contents, Token.Type expectedType, bool allowNext = false, bool allowRegex = false) {
            ErrorSinkRef errorSink = ErrorSinkRef.make!ErrorSinkRef_Console();
            assert(!errorSink.isNull);
            Lexer_Javascript lexer = Lexer_Javascript(String_UTF8(filename), String_UTF8(contents), errorSink);

            auto got = lexer.token(allowRegex);

            if(errorSink.haveError || got.type != expectedType) {
                debugWriteln(got);
                assert(0);
            }

            lexer.popFront();

            if(errorSink.haveError || (!allowNext && lexer.front != Token.Type.EndOfFile)) {
                debugWriteln(lexer.token);
                assert(0);
            }

            return got;
        }

        Token testSuccess(string mod = __MODULE__, int line = __LINE__)(string contents, Token.Type expectedType,
                bool allowNext = false, bool allowRegex = false) {
            return testSuccess2(text(mod, ":", line), contents, expectedType, allowNext, allowRegex);
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

        void testSuccessPunctuation(string mod = __MODULE__, int line = __LINE__)(string contents, wchar expected, bool allowNext = false) {
            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.Punctuation, allowNext);

            if(token.punctuation != expected) {
                debugWriteln(token, Lexer_Javascript.punctuationToText(token.punctuation), contents);
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

        void testSuccessString(string mod = __MODULE__, int line = __LINE__)(string contents, string expected) {
            import sidero.base.math.utils;

            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.String);

            if(token.text != expected) {
                debugWriteln(token);
                assert(0);
            }
        }

        void testSuccessTemplateString(string mod = __MODULE__, int line = __LINE__)(string contents, string[] texts...) {
            assert(texts.length > 1 && texts.length % 2 == 1);
            string filename = text(mod, ":", line);

            ErrorSinkRef errorSink = ErrorSinkRef.make!ErrorSinkRef_Console();
            assert(!errorSink.isNull);
            Lexer_Javascript lexer = Lexer_Javascript(String_UTF8(filename), String_UTF8(contents), errorSink);

            {
                auto got = lexer.token;

                if(errorSink.haveError || got.type != Token.Type.TemplateSubstitutionHead) {
                    debugWriteln(got);
                    assert(0);
                }

                if(errorSink.haveError || got.text != texts[0]) {
                    debugWriteln(got);
                    assert(0);
                }

                lexer.popFront();
            }

            for(size_t i = 1; i + 2 < texts.length; i += 2) {
                {
                    auto got = lexer.token;

                    if(errorSink.haveError || got.type != Token.Type.Identifier) {
                        debugWriteln(got);
                        assert(0);
                    }

                    if(errorSink.haveError || got.text != texts[i]) {
                        debugWriteln(got, texts[i]);
                        assert(0);
                    }

                    lexer.popFront();
                }

                {
                    auto got = lexer.token;

                    if(errorSink.haveError || got.type != Token.Type.TemplateSubstitutionMiddle) {
                        debugWriteln(got);
                        assert(0);
                    }

                    if(errorSink.haveError || got.text != texts[i + 1]) {
                        debugWriteln(got, texts[i + 1]);
                        assert(0);
                    }

                    lexer.popFront();
                }
            }

            {
                auto got = lexer.token;

                if(errorSink.haveError || got.type != Token.Type.Identifier) {
                    debugWriteln(got);
                    assert(0);
                }

                if(errorSink.haveError || got.text != texts[$ - 2]) {
                    debugWriteln(got, texts[$ - 2]);
                    assert(0);
                }

                lexer.popFront();
            }

            {
                auto got = lexer.token;

                if(errorSink.haveError || got.type != Token.Type.TemplateSubstitutionTail) {
                    debugWriteln(got);
                    assert(0);
                }

                if(errorSink.haveError || got.text != texts[$ - 1]) {
                    debugWriteln(got, texts[$ - 1]);
                    assert(0);
                }

                lexer.popFront();
            }

            if(errorSink.haveError || lexer.front != Token.Type.EndOfFile) {
                debugWriteln(lexer.token);
                assert(0);
            }
        }

        void testSuccessRegexString(string mod = __MODULE__, int line = __LINE__)(string contents) {
            import sidero.base.math.utils;

            Lexer_Javascript.Token token = testSuccess!(mod, line)(contents, Token.Type.RegexString, false, true);

            if(token.text != contents) {
                debugWriteln(token, contents);
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

    // +dec
    Check.testSuccessNumber("+0", 0);
    Check.testSuccessNumber("+00", 0);
    Check.testSuccessNumber("+01", 1);
    Check.testSuccessNumber("+00", 0);
    Check.testSuccessNumber("+01", 1);
    Check.testSuccessNumber("+011", 11);
    Check.testSuccessNumber("+01_1", 11);

    // -dec
    Check.testSuccessNumber("-0", 0);
    Check.testSuccessNumber("-00", 0);
    Check.testSuccessNumber("-01", -1);
    Check.testSuccessNumber("-00", 0);
    Check.testSuccessNumber("-01", -1);
    Check.testSuccessNumber("-011", -11);
    Check.testSuccessNumber("-01_1", -11);

    // dec > 0
    Check.testSuccessNumber("1", 1);
    Check.testSuccessNumber("11", 11);
    Check.testSuccessNumber("1_1", 11);

    // +dec > 0
    Check.testSuccessNumber("+1", 1);
    Check.testSuccessNumber("+11", 11);
    Check.testSuccessNumber("+1_1", 11);

    // -dec > 0
    Check.testSuccessNumber("-1", -1);
    Check.testSuccessNumber("-11", -11);
    Check.testSuccessNumber("-1_1", -11);

    //n
    Check.testSuccessBigInteger("0n", DynamicBigInteger.parse("0"));
    Check.testSuccessBigInteger("00n", DynamicBigInteger.parse("0"));
    Check.testSuccessBigInteger("01n", DynamicBigInteger.parse("1"));
    Check.testSuccessBigInteger("00n", DynamicBigInteger.parse("0"));
    Check.testSuccessBigInteger("01n", DynamicBigInteger.parse("1"));
    Check.testSuccessBigInteger("011n", DynamicBigInteger.parse("11"));
    Check.testSuccessBigInteger("01_1n", DynamicBigInteger.parse("11"));

    // +n
    Check.testSuccessBigInteger("+0n", DynamicBigInteger.parse("0"));
    Check.testSuccessBigInteger("+00n", DynamicBigInteger.parse("0"));
    Check.testSuccessBigInteger("+01n", DynamicBigInteger.parse("1"));
    Check.testSuccessBigInteger("+00n", DynamicBigInteger.parse("0"));
    Check.testSuccessBigInteger("+01n", DynamicBigInteger.parse("1"));
    Check.testSuccessBigInteger("+011n", DynamicBigInteger.parse("11"));
    Check.testSuccessBigInteger("+01_1n", DynamicBigInteger.parse("11"));

    // -n
    Check.testSuccessBigInteger("-0n", DynamicBigInteger.parse("-0"));
    Check.testSuccessBigInteger("-00n", DynamicBigInteger.parse("-0"));
    Check.testSuccessBigInteger("-01n", DynamicBigInteger.parse("-1"));
    Check.testSuccessBigInteger("-00n", DynamicBigInteger.parse("-0"));
    Check.testSuccessBigInteger("-01n", DynamicBigInteger.parse("-1"));
    Check.testSuccessBigInteger("-011n", DynamicBigInteger.parse("-11"));
    Check.testSuccessBigInteger("-01_1n", DynamicBigInteger.parse("-11"));

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

    // Multi-char punctuation
    static foreach(m; __traits(allMembers, Lexer_Javascript.PunctuationChars)[0 .. $ - 1]) {
        Check.testSuccessPunctuation(Lexer_Javascript.punctuationToText(__traits(getMember,
                Lexer_Javascript.PunctuationChars, m)), __traits(getMember, Lexer_Javascript.PunctuationChars, m));
    }

    // multi line comment
    Check.testSuccessMultiLineComment("/*some text*/", "some text");
    Check.testSuccessMultiLineComment("/* some text*/", " some text");
    Check.testSuccessMultiLineComment("/* some text\r*/", " some text\r");
    Check.testSuccessMultiLineComment("/* some text\n*/", " some text\n");
    Check.testSuccessMultiLineComment("/* some text\r\n*/", " some text\r\n");
    Check.testSuccessMultiLineComment("/* some text*/\r\n", " some text");
    Check.testSuccessMultiLineComment("/* some text*/\n", " some text");

    // single quote string
    Check.testSuccessString("'b\\\ne'", "be");
    Check.testSuccessString("'bae'", "bae");
    Check.testSuccessString("'b\ne'", "b\ne");
    Check.testSuccessString("'b\\ne'", "b\ne");
    Check.testSuccessString("'b\\be'", "b\be");
    Check.testSuccessString("'b\\u0053e'", "bSe");
    Check.testSuccessString("'b\\u{000054}e'", "bTe");
    Check.testSuccessString("'b\\uD835\\uDD04e'", "b\U0001D504e");

    // double quote strings
    Check.testSuccessString("\"b\\\ne\"", "be");
    Check.testSuccessString("\"bae\"", "bae");
    Check.testSuccessString("\"b\ne\"", "b\ne");
    Check.testSuccessString("\"b\\ne\"", "b\ne");
    Check.testSuccessString("\"b\\be\"", "b\be");
    Check.testSuccessString("\"b\\u0053e\"", "bSe");
    Check.testSuccessString("\"b\\u{000054}e\"", "bTe");
    Check.testSuccessString("\"b\\uD835\\uDD04e\"", "b\U0001D504e");

    // templated strings
    Check.testSuccessString("`thing`", "thing");
    Check.testSuccessTemplateString("`b${ident}e`", "b", "ident", "e");
    Check.testSuccessTemplateString("`b${ident}mi${another}e`", "b", "ident", "mi", "another", "e");
    Check.testSuccessTemplateString("`b${ident}mi${another}but${tok}e`", "b", "ident", "mi", "another", "but", "tok", "e");

    // regex string
    Check.testSuccessRegexString("/a/");
    Check.testSuccessRegexString("/\\a/");
    Check.testSuccessRegexString("/[a]/");

    Check.testSuccessRegexString("/ab/");
    Check.testSuccessRegexString("/\\a\\b/");
    Check.testSuccessRegexString("/[a][b]/");

    Check.testSuccessRegexString("/oops I did\\/ it again/");

    // regex string flags
    Check.testSuccessRegexString("/a/c");
    Check.testSuccessRegexString("/a/$");

    Check.testSuccessRegexString("/ab/c");
    Check.testSuccessRegexString("/ab/$");

    Check.testSuccessRegexString("/oops I did\\/ it again/vali$d");
}
