package project;

/**
 * Test class for game-selling theme
 * @author andand5
 */
public class TestClass {
    public static void main(String[] args) {

        DynamicArrayClass<String> dynamicArray = new DynamicArrayClass<>();

        // Adding elements to the dynamic array test
        dynamicArray.add("The Legend of Zelda");
        dynamicArray.add("Super Mario Odyssey");
        dynamicArray.add("Minecraft");
        System.out.println("Added games: The Legend of Zelda, Super Mario Odyssey, Minecraft");

        // Add test
        dynamicArray.add(1, "Elden Ring");
        System.out.println("Added 'Elden Ring' at index 1");

        // Array size test
        System.out.println("Size: " + dynamicArray.size()); //4

        // Get test
        System.out.println("Game at index 0: " + dynamicArray.get(0)); //The Legend of Zelda
        System.out.println("Game at index 1: " + dynamicArray.get(1)); //Elden Ring

        // Contains test
        System.out.println("Contains 'Super Mario Odyssey': " + dynamicArray.contains("Super Mario Odyssey")); //true
        System.out.println("Contains 'Fortnite': " + dynamicArray.contains("Fortnite"));   //false

        // IndexOf test
        System.out.println("Index of 'Minecraft': " + dynamicArray.indexOf("Minecraft")); //3
        System.out.println("Index of 'Fortnite': " + dynamicArray.indexOf("Fortnite"));   //-1

        // Remove by index test
        System.out.println("Removed game at index 1: " + dynamicArray.remove(1)); //Elden Ring
        System.out.println("After removing 'Elden Ring':");
        for (int i = 0; i < dynamicArray.size(); i++) {
            System.out.println(dynamicArray.get(i)); //The Legend of Zelda, Super Mario Odyssey, Minecraft
        }

        // Unset by index test
        dynamicArray.unset(1); // Unset the element at index 1
        System.out.println("Unset game at index 1");
        for (int i = 0; i < dynamicArray.size(); i++) {
            System.out.println("Index " + i + ": " + dynamicArray.get(i)); //The Legend of Zelda, null, Minecraft
        }

        System.out.println("Is empty: " + dynamicArray.isEmpty()); //false

        dynamicArray.remove(0); // Remove The Legend of Zelda
        dynamicArray.remove(0); // Remove null
        dynamicArray.remove(0); // Remove Minecraft
        System.out.println("Removed all games");
        System.out.println("Is empty after removal: " + dynamicArray.isEmpty()); //true
    }
}
