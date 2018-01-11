public class Passwo1rdSafe {
    
    char get(int index);
    int length;
    
    public int equals (Passwo1rdSafe o1, Passwo1rdSafe o2) {
        //compare passwords
        int bad = 0;
        for (int j = 0; j < o1.length; j++) {
            if (bad == 0) {
                if (j == o2.length) { bad = 1; }
                else if (o2.get(j) != o1.get(j)) { bad = 1; }
            }
        }
        
        return j;
    }
    
}
