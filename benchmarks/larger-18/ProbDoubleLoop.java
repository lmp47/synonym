public class ObjArray<Object>{
  String get_b1 (Object o);
  String get_b2 (Object o);
  String get_b3 (Object o);
  String get_b4 (Object o);
  String get_b5 (Object o);
  String get_b6 (Object o);
  String get_b7 (Object o);
  String get_b8 (Object o);
  String get_b9 (Object o);
  IntArray get_arr (Object o);

  int alength(IntArray a);
  int aget(int i);

  int length();
  Object get(int i);

  boolean is_set_b1(Object o);
  boolean is_set_b2(Object o);
  boolean is_set_b3(Object o);
  boolean is_set_b4(Object o);
  boolean is_set_b5(Object o);
  boolean is_set_b6(Object o);
  boolean is_set_b7(Object o);
  boolean is_set_b8(Object o);
  boolean is_set_b9(Object o);
  boolean is_set_arr(Object o);

  int tmp;

  public boolean equals(ObjArray<Object> o1, ObjArray<Object> o2) {
    if (o1.length != o2.length) {
      return false;
    }
    int i = 0;
    int j = 0;
    Object this_obj, that_obj;
    boolean this_b, that_b, this_arr, that_arr;
    int this_arr_len, that_arr_len;
    while (i < o1.length) {
      this_obj = o1.get(i);
      that_obj = o2.get(i);
      this_b = true && o1.is_set_b1(this_obj);
      that_b = true && o2.is_set_b1(that_obj);
      if ((this_b || that_b) && !(this_b && that_b)) {
        return false;
      }
      if ((this_b || that_b) && (equals(o1.get_b1(this_obj),o2.get_b1(that_obj)) == 0)) {
        return false;
      }
      this_b = true && o1.is_set_b2(this_obj);
      that_b = true && o2.is_set_b2(that_obj);
      if ((this_b || that_b) && !(this_b && that_b)) {
        return false;
      }
      if ((this_b || that_b) && (equals(o1.get_b2(this_obj),o2.get_b2(that_obj)) == 0)) {
        return false;
      }
      this_arr = true && o1.is_set_arr(this_obj);
      that_arr = true && o2.is_set_arr(that_obj);
      if ((this_arr || that_arr) && (!(this_arr && that_arr))) {
          return false;
      }
      this_arr_len = o1.alength(o1.get_arr(this_obj));
      that_arr_len = o2.alength(o2.get_arr(that_obj));
      if (this_arr_len != that_arr_len) {
        return false;
      }
      assume(o1.tmp == i);
      i = 0;
      while (i < this_arr_len) {
        if (o1.aget(i) != o2.aget(i)) {
          return false;
        }
        i++;
      }
    i = o1.tmp;
    i++;
    }

    return true;
  }
}

