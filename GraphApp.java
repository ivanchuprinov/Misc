import java.util.*;

public class GraphApp{
	public static List<String> findItinerary(String[][] tickets){
		int airportCount = 0;	// Number of airports in an itinerary
		Map<Integer, String> APmapIS = new HashMap<>();	// Airport = key, Index = value
		Map<String, Integer> APmapSI = new HashMap<>();	// Index = key, Airport = value
		// Fill hashmaps
		for(int i = 0; i<tickets.length; i++){
			if(!APmapIS.containsValue(tickets[i][0])){
				APmapIS.put(airportCount, tickets[i][0]);
				APmapSI.put(tickets[i][1], airportCount);
				airportCount++;
			}
			if(!APmapIS.containsValue(tickets[i][1])){
				APmapIS.put(airportCount, tickets[i][1]);
				APmapSI.put(tickets[i][1], airportCount);
				airportCount++;
			}
		}
	
		// Graph is a set of verticies and set of edges
		List<Vertex> graph = new ArrayList <Vertex>();
		
		// Create verticies
		for(int i = 0; i<airportCount; i++){
			graph.add(new Vertex(APmapIS.get(i)));
		}
		
		// Create edges
		for(int i = 0; i<tickets.length; i++){
			graph.get(APmapSI.get(tickets[i][0])).neighbors.add(graph.get(APmapSI.get(tickets[i][1])));
		}
		
		// Stack to store values
		Stack<String> st = new LinkedList<String>();
		// Visit the JFK vertex and everything it points to
		for(int i = 0; i<graph.size(); i++){
			Vertex v = graph.get(i);
			if(v.value.equals("JFK"))
				visit(v, st);
		}
		
		List<String> result = new ArrayList<String>();
		while(!st.isEmpty())
			result.add(st.pop());
		return result;
	}
	
	private static void visit(Vertex v, Stack<String> st){
		if(!v.visit.equals("false"))
			return;
		v.visit = "temporary";
		for(int i = 0; i<v.neighbors.size(); i++)
			visit(v.neighbors.get(i), st);
		v.visit = "permanent";
		st.push(v.value);
	}
}

class Vertex{
	public String value;
	public String visit;
	public List<Vertex> neighbors;
	
	public Vertex(String val){ 
		value = val;
		visit = "false";
		neighbors = new ArrayList<Vertex>();
	}
}
