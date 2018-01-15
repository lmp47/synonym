public class IFUnsafe16 {
    int length;
    char get1(int index);
    char get2(int index);
    char get3(int index);
    char get4(int index);
    char get5(int index);
    char get6(int index);
    char get7(int index);
    char get8(int index);
    char get9(int index);
    char get10(int index);
    char get11(int index);
    char get12(int index);
    char get13(int index);
    char get14(int index);
    char get15(int index);
    char get16(int index);
    public int equals (IFUnsafe16 o1, IFUnsafe16 o2, IFUnsafe16 priv) {
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
            if (o1.get9(i) != o2.get9(i)) {
                return 0;
            }
            if (o1.get10(i) != o2.get10(i)) {
                return 0;
            }
            if (o1.get11(i) != o2.get11(i)) {
                return 0;
            }
            if (o1.get12(i) != o2.get12(i)) {
                return 0;
            }
            if (o1.get13(i) != o2.get13(i)) {
                return 0;
            }
            if (o1.get14(i) != o2.get14(i)) {
                return 0;
            }
            if (o1.get15(i) != o2.get15(i)) {
                return 0;
            }
            if (o1.get16(i) != o2.get16(i)) {
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
            if (o1.get9(i) == priv.get9(i)) {
                return 0;
            }
            if (o1.get10(i) == priv.get10(i)) {
                return 0;
            }
            if (o1.get11(i) == priv.get11(i)) {
                return 0;
            }
            if (o1.get12(i) == priv.get12(i)) {
                return 0;
            }
            if (o1.get13(i) == priv.get13(i)) {
                return 0;
            }
            if (o1.get14(i) == priv.get14(i)) {
                return 0;
            }
            if (o1.get15(i) == priv.get15(i)) {
                return 0;
            }
            if (o1.get16(i) == priv.get16(i)) {
                return 0;
            }
            i++;
        }
        return 1;
    }
}