package com.ariezlabs;

import java.util.Random;

public class Knapsack {
    int capacity;
    Item[] items;

    public Knapsack(int capacity) {
        int minVal = 1;
        int maxVal = (int) (capacity*0.25);
        int nitems = capacity * 2;

        this.capacity = capacity;
        items = new Item[nitems];
        for (int i = 0; i < nitems; i++) {
            items[i] = new Item(minVal, maxVal);
        }
    }

    public Item getItem(int i) {
        return items[i];
    }

    public String toString() {
        StringBuilder sb = new StringBuilder("Knapsack(\n");
        for (Item i : items)
            sb.append("\t").append(i).append("\n");
        return sb.append(")").toString();
    }

    public static class Item {
        int value;
        int weight;
        double worth;
        private int id;
        private static int idCounter = 0;
        private static Random r = new Random();

        public Item(int a, int b) {
            this.worth = 2 * Math.random();// (1 + Math.exp(r.nextGaussian()));
            double weight = b * Math.random();// (1 + Math.exp(r.nextGaussian()));
            this.value = (int) (worth * weight);
            this.weight = (int) weight;
            this.id = idCounter++;
        }

        public String toString() {
            return String.format("%2.2f @ %d", worth, weight);
            //return String.format("id: %d\tv: %d\tw: %d", id, value, weight);
        }

        public double getWorth() {
            return worth;
        }
    }
}
