module sidero.fileformats.errors;
import sidero.fileformats.lexers.defs : Loc;
import sidero.base.allocators.classes;
import sidero.base.text;

alias ErrorSinkRef = CRef!(ErrorSink!());
alias ErrorSinkRef_Console = CRef!(ErrorSink_Console!());

///
export extern (C++) class ErrorSink() : RootRefRCClass!() {
    bool haveError;
    bool gagged;

export @safe nothrow @nogc:

    ///
    void error(Loc location, String_UTF8 message) {
        this.haveError = true;
    }

    ///
    void errorSupplimental(String_UTF8 message) {
        this.haveError = true;
    }
}

///
export extern (C++) class ErrorSink_Console() : RootRefRCClass!() {
    import sidero.base.console;

    bool haveError;
    bool gagged;

export @safe nothrow @nogc:

    ///
    void error(Loc location, String_UTF8 message) {
        this.haveError = true;

        if(!gagged)
            writeln(useErrorStream(true), location.fileName, ":", location.lineNumber, ": error: ", message);
    }

    ///
    void errorSupplimental(String_UTF8 message) {
        this.haveError = true;
        if(!gagged)
            writeln(useErrorStream(true), "    ", message);
    }
}
