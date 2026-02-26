package project;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;
import java.util.ArrayList;

/**
 * Dynamic array class for managing a collection of games in a game store.
 * @author andand5
 * @param <T> Type of elements stored, expected to be Game in this context.
 */
public class DynamicArrayClass<T> implements DynamicArrayIF<T> {
    private T[] array;
    private int size;

    @SuppressWarnings("")
    public DynamicArrayClass() {
        array = (T[]) new Object[10];
        size = 0;
    }

    @Override
    public void add(T element) {
        ensureCapacity();
        array[size++] = element;
    }

    @Override
    public void add(int index, T element) {
        if (index < 0 || index > size) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size);
        }
        ensureCapacity();
        System.arraycopy(array, index, array, index + 1, size - index);
        array[index] = element;
        size++;
    }

    @Override
    public T get(int index) {
        if (index < 0 || index >= size) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size);
        }
        return array[index];
    }

    @Override
    public T remove(int index) {
        if (index < 0 || index >= size) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size);
        }
        T removedElement = array[index];
        System.arraycopy(array, index + 1, array, index, size - index - 1);
        array[--size] = null; // Clear reference
        return removedElement;
    }

    @Override
    public boolean contains(T element) {
        return indexOf(element) != -1;
    }

    @Override
    public int indexOf(T element) {
        for (int i = 0; i < size; i++) {
            if ((element == null && array[i] == null) || (element != null && element.equals(array[i]))) {
                return i;
            }
        }
        return -1;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    /**
     * Ensures the array has enough capacity to add new elements.
     * If the array is full, it doubles its size.
     */
    private void ensureCapacity() {
        if (size == array.length) {
            array = Arrays.copyOf(array, array.length * 2);
        }
    }

    /**
     * Retrieves all games in the collection.
     * @return A list of all games.
     */
    public List<T> getAllItems() {
        return Arrays.asList(Arrays.copyOf(array, size));
    }

    /**
     * Groups games by their genre (category).
     * @return A map where the key is the genre and the value is the list of games in that genre.
     */
    public Map<String, List<T>> groupByGenre() {
        Map<String, List<T>> groupedItems = new HashMap<>();
        for (T element : getAllItems()) {
            if (element instanceof Game) {
                Game game = (Game) element;
                groupedItems.computeIfAbsent(game.getCategory(), k -> new ArrayList<>()).add((T) game);
            }
        }
        return groupedItems;
    }

    /**
     * Filters games by price range.
     * @param minPrice Minimum price of the game.
     * @param maxPrice Maximum price of the game.
     * @return A list of games within the specified price range.
     */
    public List<T> filterByPrice(double minPrice, double maxPrice) {
        List<T> filteredGames = new ArrayList<>();
        for (T element : getAllItems()) {
            if (element instanceof Game) {
                Game game = (Game) element;
                if (game.getPrice() >= minPrice && game.getPrice() <= maxPrice) {
                    filteredGames.add((T) game);
                }
            }
        }
        return filteredGames;
    }

    /**
     * Removes all games in a specific genre.
     * @param genre The genre of games to remove.
     */
    public void removeGamesByGenre(String genre) {
        for (int i = 0; i < size; i++) {
            if (array[i] instanceof Game) {
                Game game = (Game) array[i];
                if (game.getCategory().equalsIgnoreCase(genre)) {
                    remove(i);
                    i--;
                }
            }
        }
    }

    /**
     * Adds a new game to the collection.
     * @param name Name of the game.
     * @param price Price of the game.
     * @param genre Genre of the game.
     */
    public void addGame(String name, double price, String genre) {
        Game newGame = new Game(name, price, genre);
        add((T) newGame);
    }

    @Override
    public void unset(int index) {
        if (index < 0 || index >= size) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size);
        }
        array[index] = null;
    }
}
