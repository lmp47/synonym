public class ProbLoop{
  private String b1;
  private String b2;
  private String b3;
  private String b4;
  private String b5;
  private String b6;
  private String b7;
  private String b8;
  private String b9;
  private Array arr1;
  private Array arr2;

  int len(Array a);
  int get(Array a, int i);

  boolean is_set_b1();
  boolean is_set_b2();
  boolean is_set_b3();
  boolean is_set_b4();
  boolean is_set_b5();
  boolean is_set_b6();
  boolean is_set_b7();
  boolean is_set_b8();
  boolean is_set_b9();
  boolean is_set_arr1();
  boolean is_set_arr2();

  public boolean equals(ProbLoop o1, ProbLoop o2) {
    boolean this_b1 = true && o1.is_set_b1();
    boolean that_b1 = true && o2.is_set_b1();
    if (this_b1 || that_b1) {
      if (!(this_b1 && that_b1))
        return false;
      if (equals(o1.b1,o2.b1) == 0)
        return false;
    }

    boolean this_b2 = true && o1.is_set_b2();
    boolean that_b2 = true && o2.is_set_b2();
    if (this_b2 || that_b2) {
      if (!(this_b2 && that_b2))
        return false;
      if (equals(o1.b2,o2.b2) == 0)
        return false;
    }

    boolean this_b3 = true && o1.is_set_b3();
    boolean that_b3 = true && o2.is_set_b3();
    if (this_b3 || that_b3) {
      if (!(this_b3 && that_b3))
        return false;
      if (equals(o1.b3,o2.b3) == 0)
        return false;
    }

    boolean this_b4 = true && o1.is_set_b4();
    boolean that_b4 = true && o2.is_set_b4();
    if (this_b4 || that_b4) {
      if (!(this_b4 && that_b4))
        return false;
      if (equals(o1.b4,o2.b4) == 0)
        return false;
    }
/*

    boolean this_b5 = true && o1.is_set_b5();
    boolean that_b5 = true && o2.is_set_b5();
    if (this_b5 || that_b5) {
      if (!(this_b5 && that_b5))
        return false;
      if (equals(o1.b5,o2.b5) == 0)
        return false;
    }
*/

    boolean this_arr1 = true && o1.is_set_arr1();
    boolean that_arr1 = true && o2.is_set_arr1();
    if (this_arr1 || that_arr1) {
      if (!(this_arr1 && that_arr1))
        return false;
      int this_arr1_len = o1.len(o1.arr1);
      int that_arr1_len = o2.len(o2.arr1);
      if (this_arr1_len != that_arr1_len) {
        return false;
      }
      int i = 0;
      while (i < this_arr1_len) {
        if (o1.get(o1.arr1, i) != o2.get(o2.arr1, i)) {
          return false;
        }
        i++;
      }
    }

    boolean this_arr2 = true && o1.is_set_arr2();
    boolean that_arr2 = true && o2.is_set_arr2();
    if (this_arr2 || that_arr2) {
      if (!(this_arr2 && that_arr2))
        return false;
      int this_arr2_len = o1.len(o1.arr2);
      int that_arr2_len = o2.len(o2.arr2);
      if (this_arr2_len != that_arr2_len) {
        return false;
      }
      int i = 0;
      while (i < this_arr2_len) {
        if (o1.get(o1.arr2, i) != o2.get(o2.arr2, i)) {
          return false;
        }
        i++;
      }
    }
    return true;
  }

}

