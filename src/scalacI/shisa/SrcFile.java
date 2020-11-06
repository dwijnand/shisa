package shisa;

import java.util.Objects;
import java.util.StringJoiner;

public final class SrcFile {
    public final String name;
    public final String content;

    public SrcFile(String name, String content) {
        this.name = name;
        this.content = content;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof SrcFile)) return false;
        SrcFile srcFile = (SrcFile) o;
        return name.equals(srcFile.name) && content.equals(srcFile.content);
    }

    public int hashCode() { return Objects.hash(name, content); }

    public String toString() {
        return new StringJoiner(", ", SrcFile.class.getSimpleName() + "[", "]")
                .add("name='" + name + "'")
                .add("content='" + content + "'")
                .toString();
    }
}
