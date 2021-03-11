package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.text.StyledEditorKit.ForegroundAction;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    private int player;
    private boolean playerSet;
    private AtomicBoolean found;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
        visited = new ConcurrentSkipListSet<>();
        found = new AtomicBoolean(); // defaults to false
        playerSet = false;

    }

    public ForkJoinSolver(Maze maze, Object start, Set<Integer> visited, Map<Integer, Integer> predecessor, AtomicBoolean found) {
        super(maze);

        this.start = (int)start; 
        this.visited = visited;
        this.found = found;

        this.predecessor = predecessor;
        playerSet = false;

    }

    @Override
    protected void initStructures() {
        predecessor = new HashMap<>();
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        // we do not want this in our recursive call?
        return parallelSearch(start);
    }

    private List<Integer> parallelSearch(int current)
    {
        // set visited
        // map predecessor - maps key node to value node which it came from
        // thread-safe sets: ConcurrentSkipListSet
        // a depth first search with forking at each maze fork

        // ConcurrentSkipListSet´s add adds current atomically if not already present, and returns boolean telling if it succeeded
        if (!visited.add(current) && found.get()){
            return null;
        }

        // If the player is not yet placed on the map, this is where we set it
        if (!playerSet) {
            player = maze.newPlayer((int)start);
            playerSet = true;
        } 
        
        // We move the player in the GUI
        maze.move(player, current);

        // If the current node is the goal, set found flag to true and return the path to the goal
        if (maze.hasGoal(current)){
            found.set(true);
            return pathFromTo(maze.start(), current);
        }

        // We get the neighbours of the current node
        TreeSet<Integer> neighbours = new TreeSet<>(maze.neighbors(current));
        Iterator<Integer> it = neighbours.iterator();
        
        // remove the already visited nodes from the set
        while (it.hasNext()){
            int nxt = it.next();
            if (visited.contains(nxt)){
                it.remove();
            } else {
                predecessor.put(nxt, current);
            }
        }

        // If there are no neighbours, do nothing and return null
        if(neighbours.isEmpty()) {
            return null;
        } 

        List<ForkJoinSolver> children = new ArrayList<>();

        // While there are unvisited neighbours, fork new processes that explore each available path
        while (neighbours.size() > 0){
            ForkJoinSolver breakOut = new ForkJoinSolver(maze, neighbours.pollFirst(), visited, new HashMap<>(predecessor), found); // lowest entries to forks because less likely to be right?
            children.add(breakOut);
            breakOut.fork();
        }

        // This is work to do for the mother thread. Not essential since it is a forkjoinPOOL, but saves one forking operation.
        List<Integer> remainsResult = parallelSearch(neighbours.pollFirst());            
        if (remainsResult != null) return remainsResult;

        // Join threads when processes finnish
        for (ForkJoinSolver child : children) {
            List<Integer> results = child.join();
            if(results != null) return results;
        }
        
        // If neither this thread nor its children found a path, return null
        return null;
    }
}
