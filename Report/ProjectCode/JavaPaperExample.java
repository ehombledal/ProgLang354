import java.util.Random;

class JavaPaperExample{
    public static void main(String[] args) {
        long startTime = System.currentTimeMillis();
        int[] randomList = new int[500000];
        Random random = new Random();
        for(int i = 0; i < 500000; i++)
        {
            randomList[i] = random.nextInt(9 - 1) + 1; //random number between 1 and 9
        }

        for (int i = 0; i < 500000; i++)
        {
            randomList[i] = randomList[i] + 1; //adds 1 to each index
        }

        int sum = 0; 
        for (int i = 0; i < 500000; i++)
        {
            sum += randomList[i];
        }
        System.out.println("Sum is: " + sum);
        long endTime = System.currentTimeMillis();
        System.out.println("Total execution time was: " + ((endTime-startTime)) + " milliseconds");

        
    }
}