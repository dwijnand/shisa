package shisa;

public interface Compiler {
    String id();
    String cmd();
    Msgs compile1(SrcFile src);
}
