import java.util.*;

public class GraphApp{
	public static List<String> findIndex(String[][] tickets){
		int airportCount = 0;
		Map<Integer, String> APmapIS = new HashMap<>();	// Airport = key, Index = value
		Map<String, Integer> APmapSI = new HashMap<>();	// Index = key, Airport = value
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
	
	
		List<Vertex> graph = new ArrayList <Vertex>();
		
		for(int i = 0; i<airportCount; i++){
			graph.add(new Vertex(APmapIS.get(i)));
		}
		// Create edges
		for(int i = 0; i<tickets.length; i++){
			graph.get(APmapSI.get(tickets[i][0])).neighbors.add(graph.get(APmapSI.get(tickets[i][1])));
		}
		
		Queue<String> q = new LinkedList<String>();
		q.add("JFK");
		for(int i = 0; i<graph.size(); i++){
			Vertex v = graph.get(i);
			if(v.visit.equals("false"))
				visit(v, q, false);
		}
		
		List<String> result = new ArrayList<String>();
		while(!q.isEmpty())
			result.add(q.remove());
		return result;
	}
	
	private static void visit(Vertex vertex, Queue<String> q, boolean JFKfound){
		if(!vertex.equals("false"))
			return;
		if(!JFKfound && !vertex.value.equals("JFK"))
			for(int i = 0; i<vertex.neighbors.size(); i++)
				visit(vertex.neighbors.get(i), q, JFKfound);
		else
			JFKfound = true;
		vertex.visit = "temporary";
		for(int i = 0; i<vertex.neighbors.size(); i++)
			visit(vertex.neighbors.get(i), q, JFKfound);
		vertex.visit = "permanent";
		q.add(vertex.value);
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
