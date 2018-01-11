public class Passwo1rdUnsafe {
    
    char get(int index);
    int length;
    
    public int equals (Passwo1rdUnsafe o1, Passwo1rdUnsafe o2) {
        //compare passwords
        int bad = 0;
        for (int j = 0; j < o1.length  && j < o2.length; j++) {
            if (bad == 0) {
                if (o2.get(j) != o1.get(j)) { bad = 1; break; }
            }
        }
        
        return j;
    }
    
}
