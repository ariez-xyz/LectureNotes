package Parser;

import ANN.NeuralNet;
import Functions.ActivationFunction;
import Functions.FunctionRepository;
import org.w3c.dom.*;
import javax.xml.parsers.*;
import java.io.*;
import java.util.Arrays;

public class ANNParser {

    /**
     * Parse a NeuralNet configuration specified in some XML file
     * @return NeuralNet with specified configuration
     */
    public NeuralNet parse(String path) throws Exception {
        File inputFile = new File(path);
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(inputFile);
        doc.getDocumentElement().normalize();
        if(!doc.getDocumentElement().getTagName().equals("neuralnet"))
            System.err.println("Warning: XML root element is not named 'neuralnet' but " + doc.getDocumentElement().getTagName());

        // parse layer sizes attribute
        int[] layerSizes;
        try {
            layerSizes = Arrays.stream(doc.getDocumentElement().getAttribute("layers").split(" ")).mapToInt(Integer::parseInt).toArray();
            if (layerSizes.length == 0)
                throw new BadXMLException("No information given on layer sizes");
        } catch (NullPointerException e) {
            throw new BadXMLException("No information given on layer sizes");
        } catch (NumberFormatException e) {
            throw new BadXMLException("Bad layer sizes attribute: " + doc.getDocumentElement().getAttribute("layers"));
        }

        // construct basic NN
        ActivationFunction[] fns = new ActivationFunction[layerSizes.length];
        Arrays.fill(fns, FunctionRepository.get(FunctionRepository.DEFAULT));
        NeuralNet net = new NeuralNet(layerSizes, fns);

        // fill with info from XML file in 3 steps:
        // 1) for each layer element, set corresponding activation fn
        NodeList docLayers = doc.getElementsByTagName("layer");
        for (int i = 0; i < docLayers.getLength(); i++) {
            Element layer = (Element) docLayers.item(i);
            ActivationFunction f = FunctionRepository.get(layer.getAttribute("function"));
            int id;
            try {
                id = Integer.parseInt(layer.getAttribute("id"));
            } catch (NumberFormatException e) {
                throw new BadXMLException(String.format("Bad layer id attribute for layer element #%d: %s", i, layer.getAttribute("id")));
            }
            net.setActivationFunction(id, f);
        }
        // TODO improve this warning
        if(docLayers.getLength() < layerSizes.length)
            System.err.println(String.format("Warning: No activation function given for %d layers, using default function '%s'", layerSizes.length - docLayers.getLength(), FunctionRepository.get(FunctionRepository.DEFAULT).toString()));

        // 2) for each weight element set according weight
        NodeList docWeights = doc.getElementsByTagName("weight");
        // TODO add warning if some weights are left undefined
        for (int i = 0; i < docWeights.getLength(); i++) {
            try {
                Element weight = (Element) docWeights.item(i);
                int layer = Integer.parseInt(weight.getAttribute("layer"));
                int neuronId = Integer.parseInt(weight.getAttribute("neuronId"));
                int inputId = Integer.parseInt(weight.getAttribute("inputId"));
                double wt = Double.parseDouble(weight.getAttribute("wt"));
                net.setWeight(layer, neuronId, inputId, wt);
            } catch (NumberFormatException e) {
                throw new BadXMLException("Bad weight element: cannot parse numbers for weight element #" + i);
            }
        }

        // 3) for each function element set according function
        NodeList docFunctions = doc.getElementsByTagName("function");
        for (int i = 0; i < docFunctions.getLength(); i++) {
            try {
                Element function = (Element) docFunctions.item(i);
                int layer = Integer.parseInt(function.getAttribute("layer"));
                int neuronId = Integer.parseInt(function.getAttribute("neuronId"));
                String type = function.getAttribute("type");
                net.setActivationFunction(layer, neuronId, FunctionRepository.get(type));
            } catch (NumberFormatException e) {
                throw new BadXMLException("Bad function element: cannot parse numbers for function element #" + i);
            }
        }

        return net;
    }
}
