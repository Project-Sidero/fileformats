/**
Memory life time strategy of this module is based upon unique ownership and moving of each value.

```d
JSONValue value = JSONValue.create(JSONValue.Type.Number);
JSONValue nextValue = value;
// value is JSONValue.init

JSONValue nextValue2 = nextValue.acquire;
// nextValue2 and nextValue are the same one, just in two different variables
```
*/
module sidero.fileformats.json;
import sidero.base.allocators;
import sidero.base.text;
import sidero.base.math.bigint;
import sidero.base.containers.dynamicarray;
import sidero.base.attributes;

/// Wrapper around the types that JSON supports as a value, along with comments.
struct JSONValue {
    package(sidero.fileformats) @PrettyPrintIgnore @hidden {
        Node* node;

        static struct Node {
            int refCount;
            RCAllocator allocator;

            Type type;
            DynamicArray!String_UTF8 attachedComments;

            union {
                JSONArray array;
                JSONObject obj;
                DynamicBigInteger bigInteger;
                double number;
                String_UTF8 text;
            }

        @safe nothrow @nogc:

             ~this() scope {
                this.cleanup;
            }

            void cleanup() scope @trusted {
                final switch(type) {
                case Type.Array:
                    this.array.destroy;
                    break;
                case Type.Object:
                    this.obj.destroy;
                    break;
                case Type.BigInteger:
                    this.bigInteger.destroy;
                    break;
                case Type.Number:
                    break;
                case Type.Text:
                    this.text.destroy;
                    break;
                }
            }

            void rc(bool addRef) scope @trusted {
                if(addRef) {
                    refCount++;
                } else if(refCount-- == 1) {
                    // no longer known about
                    RCAllocator allocator = this.allocator;
                    allocator.dispose(&this);
                }
            }
        }
    }

export @safe nothrow @nogc:

    this(return scope ref JSONValue other) scope {
        this.node = other.node;
        other.node = null;
    }

    ~this() scope {
        if(this.node !is null)
            this.node.rc(false);
    }

    ///
    bool isNull() scope const {
        return this.node is null;
    }

    ///
    JSONValue acquire() return scope {
        if(isNull)
            return JSONValue.init;
        this.node.rc(true);
        return this;
    }

    ///
    void opAssign(return scope ref JSONValue other) scope @trusted {
        this.destroy;
        this.__ctor(other);
    }

    ///
    void opAssign(double value) scope {
        if(isNull)
            return;
        this.node.cleanup();

        this.node.type = Type.Number;
        this.node.number = value;
    }

    ///
    void opAssign(DynamicBigInteger value) scope @trusted {
        if(isNull)
            return;
        this.node.cleanup();

        this.node.type = Type.BigInteger;
        this.node.bigInteger = value;
    }

    ///
    void opAssign(String_UTF8 value) scope @trusted {
        if(isNull)
            return;
        this.node.cleanup();

        this.node.type = Type.Text;
        this.node.text = value;
    }

    ///
    void resetTo(Type type) scope @trusted {
        if(isNull)
            return;

        this.node.cleanup();
        this.node.type = type;

        final switch(type) {
        case Type.Array:
            this.node.array = JSONArray.init;
            break;
        case Type.Object:
            this.node.obj = JSONObject.init;
            break;
        case Type.BigInteger:
            this.node.bigInteger = DynamicBigInteger.init;
            break;
        case Type.Number:
            this.node.number = double.nan;
            break;
        case Type.Text:
            this.node.text = String_UTF8.init;
            break;
        }
    }

    ///
    bool haveAttachedComments() scope {
        if(isNull)
            return false;
        else
            return this.node.attachedComments.isNull;
    }

    ///
    DynamicArray!String_UTF8 attachedComments() return scope {
        if(isNull)
            return typeof(return).init;

        if(this.node.attachedComments.isNull)
            this.node.attachedComments = DynamicArray!String_UTF8(this.node.allocator);

        return this.node.attachedComments;
    }

    ///
    void match(scope void delegate(JSONArray) @safe nothrow @nogc arrayDel,
            scope void delegate(JSONObject) @safe nothrow @nogc objectDel,
            scope void delegate(DynamicBigInteger) @safe nothrow @nogc bigIntegerDel,
            scope void delegate(double) @safe nothrow @nogc numberDel,
            scope void delegate(String_UTF8) @safe nothrow @nogc textDel, scope void delegate() @safe nothrow @nogc nullDel = null) scope @trusted {

        if(isNull) {
            if(nullDel !is null)
                nullDel();
            return;
        }

        final switch(this.node.type) {
        case Type.Array:
            if(arrayDel !is null)
                arrayDel(this.node.array);
            break;
        case Type.Object:
            if(objectDel !is null)
                objectDel(this.node.obj);
            break;
        case Type.BigInteger:
            if(bigIntegerDel !is null)
                bigIntegerDel(this.node.bigInteger);
            break;
        case Type.Number:
            if(numberDel !is null)
                numberDel(this.node.number);
            break;
        case Type.Text:
            if(textDel !is null)
                textDel(this.node.text);
            break;
        }
    }

    ///
    static JSONValue create(Type type) @trusted {
        RCAllocator allocator = globalAllocator();
        JSONValue ret;

        ret.node = allocator.make!Node(1, allocator, type);

        final switch(type) {
        case Type.Array:
            ret.node.array = JSONArray.init;
            break;
        case Type.Object:
            ret.node.obj = JSONObject.init;
            break;
        case Type.BigInteger:
            ret.node.bigInteger = DynamicBigInteger.init;
            break;
        case Type.Number:
            ret.node.number = double.nan;
            break;
        case Type.Text:
            ret.node.text = String_UTF8.init;
            break;
        }

        return ret;
    }

    ///
    ulong toHash() scope const {
        return cast(ulong)this.node;
    }

    ///
    bool opEquals(scope JSONValue other) scope const {
        return this.node is other.node;
    }

    ///
    int opCmp(scope JSONValue other) scope const {
        if(this.node is other.node)
            return 0;
        else if(this.node < other.node)
            return -1;
        else
            return 1;
    }

    ///
    String_UTF8 toString(RCAllocator allocator = RCAllocator.init) @trusted {
        StringBuilder_UTF8 ret = StringBuilder_UTF8(allocator);
        toString(ret);
        return ret.asReadOnly;
    }

    ///
    void toString(Sink)(scope ref Sink sink) @trusted {
        if(isNull) {
            sink ~= "JSONValue(null)";
            return;
        }

        sink.formattedWrite("JSONValue({:p}, type={:s}, have comments={:s})", this.node, this.node.type, this.haveAttachedComments());
    }

    ///
    String_UTF8 toStringPretty(RCAllocator allocator = RCAllocator.init) @trusted {
        StringBuilder_UTF8 ret = StringBuilder_UTF8(allocator);
        toStringPretty(ret);
        return ret.asReadOnly;
    }

    ///
    void toStringPretty(Sink)(scope ref Sink sink) @trusted {
        if(isNull) {
            sink ~= "JSONValue(null)";
            return;
        }

        sink.formattedWrite("JSONValue(@{:p}, type={:s} =>", this.node, this.node.type);

        this.match((JSONArray) {}, (JSONObject) {}, (DynamicBigInteger bigInteger) { sink.formattedWrite(" {:s}"); }, (double number) {
            sink.formattedWrite(" {:s}", number);
        }, (String_UTF8 text) { sink ~= "\n"; PrettyPrint pp = PrettyPrint.defaults; pp.useQuotes = true; pp(sink, text); });

        if(this.haveAttachedComments()) {
            sink ~= ",\ncomments =>\n";

            this.attachedComments().toStringPretty(sink);

            sink ~= ")";
        } else
            sink ~= ", have comments=false)";

    }

    ///
    enum Type {
        Array, ///
        Object, ///
        BigInteger, ///
        Number, ///
        Text ///
    }
}

///
unittest {
    import sidero.base.math.utils;

    JSONValue value = JSONValue.create(JSONValue.Type.Number);
    assert(!value.isNull);

    value.attachedComments() ~= String_UTF8("Some comment goes here");
    assert(!value.isNull);

    value = 42;
    value.match((JSONArray) => assert(0), (JSONObject) => assert(0), (DynamicBigInteger) => assert(0),
            (double number) => assert(isClose(number, 42)), (String_UTF8) => assert(0), ()=> assert(0));

    value = DynamicBigInteger.parse("95");
    value.match((JSONArray) => assert(0), (JSONObject) => assert(0),
            (DynamicBigInteger bigInteger) => assert(bigInteger == DynamicBigInteger.parse("95")),
            (double) => assert(0), (String_UTF8) => assert(0), ()=> assert(0));

    value = String_UTF8("Hi there!");
    value.match((JSONArray) => assert(0), (JSONObject) => assert(0), (DynamicBigInteger) => assert(0),
            (double) => assert(0), (String_UTF8 text) => assert(text == "Hi there!"), ()=> assert(0));

    assert(value.attachedComments() == [String_UTF8("Some comment goes here")]);
}

struct JSONArray {

}

struct JSONObject {

}

package(sidero.fileformats):

struct JSONState {
    int refCount;
    RCAllocator allocator;
}
