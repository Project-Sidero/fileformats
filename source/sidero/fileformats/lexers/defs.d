module sidero.fileformats.lexers.defs;
import sidero.base.text;

///
struct Loc {
    ///
    String_UTF8 fileName;
    ///
    uint lineNumber;
    ///
    uint lineOffset;

export @safe nothrow @nogc:

    this(return scope ref Loc other) scope {
        this.tupleof = other.tupleof;
    }

    ~this() scope {
    }

    void opAssign(return scope Loc other) scope {
        this.destroy;
        this.__ctor(other);
    }
}
