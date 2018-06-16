/*
 * Based on http://stackoverflow.com/questions/23907134/comparing-two-arrays-using-dictionary-order-in-an-array-of-arrays-java
 * 
 */

public class AInt implements Comparator<AInt>{
   int length;
   int get(int pos);
   int id;
   int min123;
   int min12;
   int min13;
   int min23;
   
   public int pick(AInt o1, AInt o2, AInt o3){
     assume (o1.id != o2.id);
     assume (o1.id != o3.id);
     assume (o2.id != o3.id);
     if (o1.id <= o2.id) {
       assume (o1.min12 == o1.id);
     } else {
       assume (o1.min12 == o2.id);
     }
     if (o1.id <= o3.id) {
       assume (o1.min13 == o1.id);
     } else {
       assume (o1.min13 == o3.id);
     }
     if (o2.id <= o3.id) {
       assume (o1.min23 == o2.id);
     } else {
       assume (o1.min23 == o3.id);
     }
     if (o1.id <= o1.min23) {
       assume (o1.min123 == o1.id);
     } else {
       assume (o1.min123 == o1.min23);
     }
     int index, index2, aentry, bentry, centry;
     index = 0;
     
     while ((index < o1.length) && ((index < o2.length) && (index < o3.length))) {
       aentry = o1.get(index);
       bentry = o2.get(index);
       centry = o3.get(index);
       if ((aentry < centry) && (bentry < centry)) {
         return o3.id;
       }
       if ((aentry < bentry) && (centry < bentry)) {
         return o2.id;
       }
       if ((bentry < aentry) && (centry < aentry)) {
         return o1.id;
       }
       index++;
     }

     if (o1.length < o2.length) {
       if (o2.length < o3.length) {
         return o3.id;
       }
       if (o2.length > o3.length) {
         return o2.id;
       }
       return o1.min23;
     }
     if (o1.length > o2.length) {
       if (o1.length < o3.length) {
         return o3.id;
       }
       if (o1.length > o3.length) {
         return o1.id;
       }
       return o1.min13;
     }
     if (o2.length > o3.length) {
       return o1.min123;
     }
     if (o3.length > o2.length) {
       return o3.id;
     }

     return o1.min123;
  }
}

