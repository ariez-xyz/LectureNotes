package com.ariezlabs;

import java.util.ArrayList;
import java.util.Arrays;

public class Main {
    // return random int in (a,b]
    public static int randInt(int a, int b) {
        return (int) (Math.random() * (b-a) + a);
    }


    static Genotype[] generatePopulation(int n, Knapsack forWorld) {
        Genotype[] gs = new Genotype[n];
        for (int i = 0; i < n; i++)
            gs[i] = new Genotype(forWorld);
        return gs;
    }

    static Genotype[] decimate(Genotype[] population) {
        double perc = topPerc;

        int n = population.length;
        int cutoff = Arrays.stream(population).mapToInt(Genotype::evalFitness).sorted().toArray()[n-(int)(n*perc)];

        ArrayList<Genotype> survivors = new ArrayList<>((int)(n*perc*1.1));
        for (Genotype g : population)
            if (g.evalFitness() >= cutoff)
                survivors.add(g);

        return survivors.toArray(new Genotype[0]);
    }

    static void mutate(Genotype[] population, Genotype[] survivors) {
        for (int i = 0; i < population.length; i++) {
            Mutator.mutate(population[i], survivors[i % survivors.length]);
        }
    }

    static double avg(Genotype[] population) {
        return Arrays.stream(population).mapToInt(Genotype::evalFitness).average().orElse(-1);
    }

    static int max(Genotype[] population) {
        return Arrays.stream(population).mapToInt(Genotype::evalFitness).max().orElse(-1);
    }

    static Genotype maxGenotype(Genotype[] population) {
        int max = max(population);
        for (int i = 0; i < population.length; i++) {
            if (population[i].evalFitness() == max)
                return new Genotype(population[i]);
        }
        return null;
    }

    static void printPopulationInfo(Genotype[] population) {
        System.out.printf("Max: %d, individuals: ", max(population));

        int printMax = 20;
        for (Genotype g : population)
            if (printMax-- >= 0)
                System.out.printf("%d ", g.evalFitness());
            else {
                System.out.print("...");
                break;
            }
        System.out.println();
    }

    public static void runExperiment(Knapsack world) {
        Genotype[] population = generatePopulation(populationSize, world);
        Genotype[] survivors;

        int max = max(population);
        Genotype maxGenotype = maxGenotype(population);

        //System.out.println(maxGenotype);
        System.out.printf("%d -> ", max(population));

        for (int i = 0; i < epochs; i++) {
            survivors = decimate(population);
            mutate(population, survivors);
            int thisgen = max(population);
            if (thisgen > max) {
                max = thisgen;
                maxGenotype = maxGenotype(population);
            }
            //System.out.printf("ath: %d, this gen: %d (avg: %f, selected avg: %f)\n", max, thisgen, avg(population), avg(survivors));
        }

        System.out.printf("%d\n", max);
        //System.out.println(maxGenotype);
    }

    public static int knapsackCapacity = 250000;
    public static int populationSize = 20;
    public static int epochs = 1000000;
    public static int itemsToSwap = 2;
    public static double topPerc = 0.1;
    public static int experiments = 5;

    public static void main(String[] args) {
        Knapsack world = new Knapsack(knapsackCapacity);
        for (int i = 0; i < experiments; i++) {
            runExperiment(world);
        }
    }
}
