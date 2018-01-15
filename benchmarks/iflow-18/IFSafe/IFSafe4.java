public class IFSafe4 {
    int length;
    char get1(int index);
    char get2(int index);
    char get3(int index);
    char get4(int index);
    public int equals (IFSafe4 o1, IFSafe4 o2, IFSafe4 priv) {
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
            if (o1.get3(i) != o2.get3(i)) {
                return 0;
            }
            if (o1.get4(i) != o2.get4(i)) {
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
            if ((o1.get3(i) == priv.get3(i)) && (o2.get3(i) == priv.get3(i))) {
                return 1;
            }
            if ((o1.get4(i) == priv.get4(i)) && (o2.get4(i) == priv.get4(i))) {
                return 1;
            }
            i++;
        }
        return 1;
    }
}