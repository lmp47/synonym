public class IFUnsafe8 {
    int length;
    char get1(int index);
    char get2(int index);
    char get3(int index);
    char get4(int index);
    char get5(int index);
    char get6(int index);
    char get7(int index);
    char get8(int index);
    public int equals (IFUnsafe8 o1, IFUnsafe8 o2, IFUnsafe8 priv) {
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
            if (o1.get5(i) != o2.get5(i)) {
                return 0;
            }
            if (o1.get6(i) != o2.get6(i)) {
                return 0;
            }
            if (o1.get7(i) != o2.get7(i)) {
                return 0;
            }
            if (o1.get8(i) != o2.get8(i)) {
                return 0;
            }
            i++;
        }
        i = 0;
        while (i < o1.length) {
            if (o1.get1(i) == priv.get1(i)) {
                return 0;
            }
            if (o1.get2(i) == priv.get2(i)) {
                return 0;
            }
            if (o1.get3(i) == priv.get3(i)) {
                return 0;
            }
            if (o1.get4(i) == priv.get4(i)) {
                return 0;
            }
            if (o1.get5(i) == priv.get5(i)) {
                return 0;
            }
            if (o1.get6(i) == priv.get6(i)) {
                return 0;
            }
            if (o1.get7(i) == priv.get7(i)) {
                return 0;
            }
            if (o1.get8(i) == priv.get8(i)) {
                return 0;
            }
            i++;
        }
        return 1;
    }
}