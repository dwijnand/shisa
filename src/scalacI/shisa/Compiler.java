package shisa;

import java.nio.file.Path;

public interface Compiler {
    String id();
    String cmd();
    Msgs compile1(Path src);
}
