public class IFSafe1 {
    int length;
    char get1(int index);
    public int equals (IFSafe1 o1, IFSafe1 o2, IFSafe1 priv) {
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
            if ((o1.get1(i) == priv.get1(i)) && (o2.get1(i) == priv.get1(i))) {
                return 1;
            }
            i++;
        }
        return 1;
    }
}