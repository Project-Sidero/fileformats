module sidero.fileformats.internal.smallarray;
import sidero.base.containers.dynamicarray;

struct SmallArray(T) {
    private {
        T[] slice;
        T[64] buffer;
        DynamicArray!T dynamic;
    }

    @disable this(this);

export @safe nothrow @nogc:

    this(size_t amount) scope @trusted {
        if(amount > buffer.length) {
            dynamic.length = amount;
            slice = dynamic.unsafeGetLiteral;
        } else {
            slice = buffer[0 .. amount];
        }
    }

    ~this() scope {
    }

    T[] get() return scope {
        return slice;
    }
}
