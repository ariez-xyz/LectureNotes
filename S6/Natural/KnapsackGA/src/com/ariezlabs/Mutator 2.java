package com.ariezlabs;

import java.util.HashSet;

public class Mutator {

    // Make g a mutation of parent, that is:
    // throw a random item out and add as many new ones as possible
    static void mutate(Genotype g, Genotype parent) {
        g.items = new HashSet<>(parent.items);
        int itemsToRemove = (int) (Math.random() * Main.itemsToSwap);
        for (int k = 0; k < itemsToRemove; k++) {
            double minWorth = Double.POSITIVE_INFINITY;
            int index = 0;
            for (Integer j : g.items) {
                if (g.world.getItem(j).getWorth() < minWorth) {
                    minWorth = g.world.getItem(j).getWorth();
                    index = j;
                }
            }

            g.items.remove(index);
        }

        while(g.tryAddItem() != null);
    }
}
