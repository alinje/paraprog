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
    int player;

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
        player = maze.newPlayer(maze.start());
    }

    public ForkJoinSolver(Maze maze, Object start, Set<Integer> visited, Map<Integer, Integer> predecessor) {
        super(maze);

        this.start = (int)start; //TODO nah
        this.visited = visited;


        player = maze.newPlayer((int)start);
        this.predecessor = predecessor;


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

    private List<Integer> parallelSearch(int startID)
    {
        // set visited
        // map predecessor - maps key node to value node which it came from
        // thread-safe sets: ConcurrentSkipListSet
        // a depth first search with forking at each maze fork

        // ConcurrentSkipListSet´s add adds current atomically if not already present, and returns boolean telling if it succeeded
        if (visited.add(startID)){

            int current = startID;
            if (maze.hasGoal(current)){
                
                maze.move(player, current);

                //TODO do we need to tell the other threads that we are done?
                
                return pathFromTo(maze.start(), current);
            }


            maze.move(player, current);

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

            if (neighbours.size() == 1){
                return parallelSearch(neighbours.first());
            } else if (neighbours.size() == 2){ //TODO possibly add condition around forkAfter()
                ForkJoinSolver breakOut = new ForkJoinSolver(maze, neighbours.pollFirst(), visited, new HashMap<>(predecessor)); // lowest entries to forks because less likely to be right?
                breakOut.fork();
                
                List<Integer> remainsResult = parallelSearch(neighbours.pollLast());
                
                if (remainsResult != null) return remainsResult;

                List<Integer> breakOutResults = breakOut.join();
                if ( breakOutResults != null) return breakOutResults; //TODO this could be done with the questionmarkthingy

            } else if (neighbours.size() == 3){ // neighbours sixe is three
                ForkJoinSolver breakOut1 = new ForkJoinSolver(maze, neighbours.pollFirst(), visited, new HashMap<>(predecessor));
                ForkJoinSolver breakOut2 = new ForkJoinSolver(maze, neighbours.pollFirst(), visited, new HashMap<>(predecessor));

                breakOut1.fork();
                breakOut2.fork();

                List<Integer> remains = parallelSearch(neighbours.pollLast());
                
                if (remains != null) return remains;

                List<Integer> breakOutResults = breakOut2.join();
                if ( breakOutResults != null) return breakOutResults;
                breakOutResults = breakOut1.join();
                if ( breakOutResults != null) return breakOutResults;
            } 
        }

        return null;
    }
}
