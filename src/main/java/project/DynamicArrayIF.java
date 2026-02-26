/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package project;

/**
 * Dynamic array interface for managing a collection of games in a game store.
 * @author aandr
 * @param <T> Type of elements stored, expected to be Game in this context.
 */
public interface DynamicArrayIF<T> {

    /**
     * Adds a new game to the collection.
     * @param element The game to add.
     */
    void add(T element);                // Add game to the end of the collection.

    /**
     * Adds a new game at the specified position in the collection.
     * @param index The position to insert the game.
     * @param element The game to add.
     */
    void add(int index, T element);     // Add game by index.

    /**
     * Retrieves the game at the specified position in the collection.
     * @param index The position of the game.
     * @return The game at the given position.
     */
    T get(int index);                   // Get a game by index.

    /**
     * Removes the game at the specified position in the collection.
     * @param index The position of the game to remove.
     * @return The removed game.
     */
    T remove(int index);                // Remove game by index.

    /**
     * Checks if the specified game is in the collection.
     * @param element The game to search for.
     * @return True if the game is in the collection, false otherwise.
     */
    boolean contains(T element);        // Check if a game exists in the collection.

    /**
     * Finds the index of the specified game in the collection.
     * @param element The game to find.
     * @return The index of the game, or -1 if it is not found.
     */
    int indexOf(T element);             // Get the index of a specific game.

    /**
     * Retrieves the number of games in the collection.
     * @return The total number of games.
     */
    int size();                         // Get the total number of games.

    /**
     * Checks if the collection of games is empty.
     * @return True if the collection is empty, false otherwise.
     */
    boolean isEmpty();                  // Check if the collection is empty.

    /**
     * Removes the game at the specified position but leaves the position intact (sets it to null).
     * @param index The position to unset.
     */
    void unset(int index);              // Remove a game but keep the index in the collection.
}
