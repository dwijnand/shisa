package shisa;

import java.util.List;
import java.util.Objects;
import java.util.StringJoiner;

public final class Msgs {
    public final List<Msg> msgs;

    public Msgs(List<Msg> msgs) {
        this.msgs = msgs;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Msgs)) return false;
        Msgs that = (Msgs) o;
        return msgs.equals(that.msgs);
    }

    public int hashCode() { return Objects.hash(msgs); }

    public String toString() {
        return new StringJoiner(", ", Msgs.class.getSimpleName() + "[", "]")
                .add("msgs=" + msgs)
                .toString();
    }
}
