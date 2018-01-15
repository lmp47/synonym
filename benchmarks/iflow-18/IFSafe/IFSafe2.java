public class IFSafe2 {
    int length;
    char get1(int index);
    char get2(int index);
    public int equals (IFSafe2 o1, IFSafe2 o2, IFSafe2 priv) {
        assume (o1.length == o2.length);
        assume (o1.length == priv.length);
        int i = 0;
        while (i < o1.length) {
            if (o1.get1(i) != o2.get1(i)) {
                return 0;
            }
            if (o1.get2(i) != o2.get2(i)) {
                return 0;
            }
            i++;
        }
        i = 0;
        while (i < o1.length) {
            if ((o1.get1(i) == priv.get1(i)) && (o2.get1(i) == priv.get1(i))) {
                return 1;
            }
            if ((o1.get2(i) == priv.get2(i)) && (o2.get2(i) == priv.get2(i))) {
                return 1;
            }
            i++;
        }
        return 1;
    }
}