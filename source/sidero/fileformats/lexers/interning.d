module sidero.fileformats.lexers.interning;
import sidero.base.containers.map.hashmap;
import sidero.base.text;

///
struct Lexer_Interning {
    private {
        HashMap!(String_UTF8, String_UTF8) interned;
    }

export @safe nothrow @nogc:

    this(return scope ref Lexer_Interning other) scope {
        this.tupleof = other.tupleof;
    }

    ~this() scope {
    }

    ///
    void preFill(scope String_UTF8[] strings...) scope {
        foreach(s; strings) {
            interned[s] = s;
        }
    }

    ///
    void preFill(scope string[] strings...) scope {
        foreach(s; strings) {
            String_UTF8 str = String_UTF8(s);
            interned[str] = str;
        }
    }

    ///
    String_UTF8 store(const(char)[] text, bool needsNormalization) scope {
        String_UTF8 input = String_UTF8(text);

        if(!needsNormalization) {
            auto got = interned[input];
            if(got)
                return got.get;

            input = input.dup;
            interned[input] = input;

            return input;
        }

        auto got = interned[input];
        if(got)
            return got.get;

        input = input.toNFC;
        interned[input] = input;

        return input;
    }
}
