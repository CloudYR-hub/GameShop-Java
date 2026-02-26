/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package project;

import java.util.ArrayList;
import java.util.List;

/**
 * Performance test class for evaluating game inventory management.
 * Tests the performance of the DynamicArrayClass and ArrayList for managing game data.
 * @author andand5
 */
public class PerformanceTest {
    public static void main(String[] args) {
        int numberOfGames = 100000; // Number of games to add and retrieve
        DynamicArrayClass<Integer> dynamicGameArray = new DynamicArrayClass<>();
        ArrayList<Integer> gameArrayList = new ArrayList<>();

        // DynamicArrayClass add test for games
        long startTime = System.nanoTime();
        for (int i = 0; i < numberOfGames; i++) {
            dynamicGameArray.add(i);
        }
        long endTime = System.nanoTime();
        System.out.println("DynamicArrayClass add (games): " + (endTime - startTime) / 1_000_000 + " ms");

        // ArrayList add test for games
        startTime = System.nanoTime();
        for (int i = 0; i < numberOfGames; i++) {
            gameArrayList.add(i);
        }
        endTime = System.nanoTime();
        System.out.println("ArrayList add (games): " + (endTime - startTime) / 1_000_000 + " ms");

        // DynamicArrayClass get test for games
        startTime = System.nanoTime();
        for (int i = 0; i < numberOfGames; i++) {
            dynamicGameArray.get(i);
        }
        endTime = System.nanoTime();
        System.out.println("DynamicArrayClass get (games): " + (endTime - startTime) / 1_000_000 + " ms");

        // ArrayList get test for games
        startTime = System.nanoTime();
        for (int i = 0; i < numberOfGames; i++) {
            gameArrayList.get(i);
        }
        endTime = System.nanoTime();
        System.out.println("ArrayList get (games): " + (endTime - startTime) / 1_000_000 + " ms");
    }
}
