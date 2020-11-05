package shisa;

import java.util.Objects;
import java.util.StringJoiner;

public final class Msg {
    public final Severity severity;
    public final String path;
    public final int lineNo;
    public final String text;

    public Msg(Severity severity, String path, int lineNo, String text) {
        this.severity = severity;
        this.path = path;
        this.lineNo = lineNo;
        this.text = text;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Msg)) return false;
        Msg msg = (Msg) o;
        return lineNo == msg.lineNo &&
                severity == msg.severity &&
                path.equals(msg.path) &&
                text.equals(msg.text);
    }

    public int hashCode() { return Objects.hash(severity, path, lineNo, text); }

    public String toString() {
        return new StringJoiner(", ", Msg.class.getSimpleName() + "[", "]")
                .add("severity=" + severity)
                .add("path='" + path + "'")
                .add("lineNo=" + lineNo)
                .add("text='" + text + "'")
                .toString();
    }
}
