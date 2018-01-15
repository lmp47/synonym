public class IFUnsafe1 {
    int length;
    char get1(int index);
    public int equals (IFUnsafe1 o1, IFUnsafe1 o2, IFUnsafe1 priv) {
        assume (o1.length == o2.length);
        assume (o1.length == priv.length);
        int i = 0;
        while (i < o1.length) {
            if (o1.get1(i) != o2.get1(i)) {
                return 0;
            }
            i++;
        }
        i = 0;
        while (i < o1.length) {
            if (o1.get1(i) == priv.get1(i)) {
                return 0;
            }
            i++;
        }
        return 1;
    }
}