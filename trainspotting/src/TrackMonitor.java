import java.util.ArrayDeque;
import java.util.Queue;
import java.util.concurrent.locks.*;

import Lab1.TrainProgram;


public class TrackMonitor {
    private final Lock trackMonitor = new ReentrantLock(true);

    //private TrackMonitor west, east;

    private Queue<TrainProgram> entryQueue = new ArrayDeque<>(); //this might be where we reach trains for trainstuff???

    private boolean clearNeck;
    private boolean clearPrimeLeg;

    private TrainProgram neckTrain;
    private TrainProgram primeTrain;
    private TrainProgram secTrain;


    private Condition neckClear = trackMonitor.newCondition();
    private Condition primeLegClear = trackMonitor.newCondition();

//needs to also check sister


  /*  private Condition speedCond = trackMonitor.newCondition();
    private Condition clearNext = trackMonitor.newCondition();
    private Condition primaryClear = trackMonitor.newCondition();
    private Condition secondaryClear = trackMonitor.newCondition();
    private Condition okayDirectionOtherTrain = trackMonitor.newCondition();
    private Condition westPrimeClear = trackMonitor.newCondition();
    private Condition eastPrimeClear = trackMonitor.newCondition();
    private boolean clear; */
    
    public void enter() throws InterruptedException {
        trackMonitor.lock();
        if (!clearNeck){
            trackMonitor.unlock();
            //stop train and block?

            clearNext.wait();
        }else{
            trackMonitor.unlock();

        }

        // train stuff, or nothing?

    }

    public void choosePartition(){
        trackMonitor.lock();
        if(clearPrimeNeck){
            

        } else {
            
        }

    }

    public void leave(TrainProgram train){
        // works both for leaving the neck and leaving the monitor
        

        trackMonitor.lock();
        // is it in the neck?
        if(train.equals(neckTrain)){
            if(primaryClear.isEmpty()){
                // enter prime track
                
            } else {
                // enter sec track

            }
        }




        primaryClear.signal(); //signal and continue??


    }


    private boolean speedUpTrack(){

    }

    private boolean checkSister(){
        return train.sister.status();
    }

    public double status(){
        if (primeTrain == null && secTrain == null) return -1;
        if (primeTrain == null) return secTrain.estTime();
        if (secTrain == null) return primeTrain.estTime();
        return Math.min(primeTrain.estTime(), secTrain.estTime());
    }
}
