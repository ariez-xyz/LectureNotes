package com.ariezlabs;

import java.util.HashSet;
import java.util.Set;

import static com.ariezlabs.Main.randInt;

public class Genotype {
    Knapsack world;
    Set<Integer> items;

    public Genotype(Genotype toCopy) {
        this.world = toCopy.world;
        this.items = new HashSet<Integer>(toCopy.items.size());
        for (int i : toCopy.items)
            this.items.add(i);
    }

    public Genotype(Knapsack world) {
        this.world = world;
        this.items = new HashSet<Integer>();

        // choose as many random items as possible
        while(tryAddItem() != null);
    }

    public Knapsack.Item tryAddItem() {
        int toAdd = randInt(0, world.items.length);
        if (world.getItem(toAdd).weight + totalWt() <= world.capacity) {
            items.add(toAdd);
            return world.getItem(toAdd);
        }
        else
            return null;
    }

    public String toString() {
        //return Arrays.toString(items.toArray());
        StringBuilder sb = new StringBuilder("Genotype " + evalFitness() + "/" + totalWt() + "(\n");
        for (int i : items)
            if (i != -1)
                sb.append("\t").append(world.getItem(i).toString()).append("\n");
        return sb.append(")").toString();
    }

    public int evalFitness() {
        int fitness = 0;
        for (int i : items)
            fitness += world.getItem(i).value;
        return fitness;
    }

    public int totalWt() {
        int wt = 0;
        for (int i : items)
            wt += world.getItem(i).weight;
        return wt;
    }
}
