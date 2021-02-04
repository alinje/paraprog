import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import TSim.*;

public class Lab1Extra {

    enum Sensor {

        NORTH_STATION_TOP("15 3"), 
        NORTH_STATION_BOT("15 5"),

        CROSSROADS_N_N("6 6"), 
        CROSSROADS_N_S("9 5"), 
        CROSSROADS_S_N("11 7"), 
        CROSSROADS_S_S("10 8"),

        MID_SINGLE_N_N("14 7"), 
        MID_SINGLE_N_S("15 8"), 
        MID_SINGLE("19 8"), 
        MID_SINGLE_S_N("12 9"), 
        MID_SINGLE_S_S("13 10"),

        SOUTH_SINGLE_N_N("7 9"), 
        SOUTH_SINGLE_N_S("6 10"), 
        SOUTH_SINGLE("1 9"), 
        SOUTH_SINGLE_S_N("6 11"),
        SOUTH_SINGLE_S_S("4 13"),

        SOUTH_STATION_TOP("15 11"), 
        SOUTH_STATION_BOT("15 13");


        String pos;

        private Sensor(String pos) {
            this.pos = pos;

        }

        public static final Map<String, Sensor> fromCoords = new HashMap<>();

        static {
            for (Sensor s: values()){
                fromCoords.put(s.pos, s);
            }
        }

        public static Sensor getSensorFromCoord(String pos){
            return fromCoords.get(pos);
        }
    }

    enum Direction {
        NORTH, SOUTH;
    }


    TrackMonitor northStation = new TrackMonitor();
    TrackMonitor midSingle = new TrackMonitor();
    TrackMonitor southSingle = new TrackMonitor();


    public Lab1Extra(int speed1, int speed2) {
        TSimInterface tsi = TSimInterface.getInstance();

        try {
	    tsi.setDebug(true);
	    tsi.setSpeed(2, speed2);
	    tsi.setSpeed(1, speed1);
        TrainProgram train1 = new TrainProgram(1, 15, 3, speed1, Direction.SOUTH);
        TrainProgram train2 = new TrainProgram(2, 15, 11, speed2, Direction.NORTH);

        // do they need an init monitor?
        train1.start();
        train2.start();

        } catch (CommandException e) {
            e.printStackTrace(); // or only e.getMessage() for the error
            System.exit(1);
        }
    }

    public class TrainProgram extends Thread {
        int trainID;
        int xPos;
        int yPos;
        int speed;

        Direction trainHead;


        private TSimInterface interf = TSimInterface.getInstance();

        public TrainProgram(int trainID, int xPos, int yPos, int initSpeed, Direction dir) {
            this.trainID = trainID;
            this.xPos = xPos;
            this.yPos = yPos;
            this.speed = initSpeed;
            this.trainHead = dir;
        }

        @Override
        public void run() {
            try {
                while (true){
                    SensorEvent es = interf.getSensor(trainID);
                    handleSensorEvent(es);
                }
            } catch (CommandException | InterruptedException e) {
                e.printStackTrace();
            }
        }

        public void handleSensorEvent(SensorEvent es) throws CommandException, InterruptedException {
            if (es.getStatus() == SensorEvent.INACTIVE) return;

            xPos = es.getXpos();
            yPos = es.getYpos();
            
            Sensor sensor = Sensor.getSensorFromCoord(xPos + " " + yPos);

            //halts the train for the time of monitor blockage
            interf.setSpeed(trainID, 0);


            switch (sensor) {
                // three different types of situations:
                // cases of train arrived at station
                case NORTH_STATION_TOP:
                case NORTH_STATION_BOT:

                    // sleep thread for station waiting time
                    sleep(2000); //TODO should this depend on speed?
                    System.out.println("i'm wide awake // ur luv " + trainID);
                    trainHead = Direction.SOUTH;
                    speed = - speed;
                    break;

                case SOUTH_STATION_TOP:
                case SOUTH_STATION_BOT:

                    sleep(2000);
                    System.out.println("i'm wide awake // ur luv " + trainID);

                    trainHead = Direction.NORTH;
                    speed = - speed;
                    break;

                // cases of train nearing new critical zone
                // enter co traindir fork
                // leave forking of contradir fork
                // set next switch acoordingly

                case MID_SINGLE_N_N:
                    if (trainHead == Direction.NORTH){
                        midSingle.leaveCoContra(this);
                    } else {
                        midSingle.enterCo(this);
                        northStation.leaveCoContra(this);
    
                        interf.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                    }

                    break;
                case MID_SINGLE_N_S:
                    if (trainHead == Direction.NORTH) {
                        midSingle.leaveCoContra(this);
                    } else {
                        midSingle.enterCo(this);
                        northStation.leaveCoContra(this);
    
                        interf.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
                    }

                    break;
                case MID_SINGLE_S_N:
                    if (trainHead == Direction.SOUTH){
                        midSingle.leaveNeck(this);
                    } else {
                        midSingle.transitionContra(this);
                        interf.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                    }
                    break;
                case MID_SINGLE_S_S:
                    if (trainHead == Direction.SOUTH) {
                        midSingle.leaveNeck(this);
                    } else {
                        midSingle.transitionContra(this);

                        interf.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                    }

                    break;

                case SOUTH_SINGLE_N_N:
                    if (trainHead == Direction.NORTH){
                        southSingle.leaveCoContra(this);
                    } else {
                        southSingle.enterCo(this);
                        midSingle.leaveCoContra(this);
    
                        interf.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                    }

                    break;
                case SOUTH_SINGLE_N_S:
                    if (trainHead == Direction.NORTH){
                        southSingle.leaveCoContra(this);
                    } else {
                        southSingle.enterCo(this);
                        midSingle.leaveCoContra(this);
    
                        interf.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                    }

                    break;
                case SOUTH_SINGLE_S_N:
                    if (trainHead == Direction.SOUTH){
                        southSingle.leaveNeck(this);
                    } else {
                        southSingle.transitionContra(this);

                        interf.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
                    }

                    break;
                case SOUTH_SINGLE_S_S:
                    if (trainHead == Direction.SOUTH){
                        southSingle.leaveNeck(this);
                    }
                    southSingle.transitionContra(this);

                    interf.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                    break;                   

                // case of train in a critical zone, nearing forking
                case MID_SINGLE:
                    if (trainHead == Direction.NORTH){
                        // sets the switch depending on whether the train got prime track access or not
                        if (northStation.requestContra(this)){
                            interf.setSwitch(17, 7, TSimInterface.SWITCH_LEFT); // south platform is the prime track of north station
                        } else {
                            interf.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
                        }
                        //TODO move to "wrongdir"
                    } else {
                        if (midSingle.requestCo(this)){
                            interf.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
                        } else {
                            interf.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
                        } // nothing to leave in south direction
                    }
                    break;

                case SOUTH_SINGLE:
                    if (trainHead == Direction.NORTH){
                        if (midSingle.requestContra(this)){
                            interf.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
                        } else {
                            interf.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
                        }
                        
                    } else {
                        //entering south station
                        if (southSingle.requestCo(this)){
                            interf.setSwitch(3, 11, TSimInterface.SWITCH_LEFT); // north platform is the prime track of south station
                        } else {
                            interf.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
                        }
                    }
                    break;


                case CROSSROADS_S_N:
                case CROSSROADS_S_S:
                    // case of train nearing crossroads from south
                    if (trainHead == Direction.NORTH){
                        northStation.enterCo(this);
                    } else {
                        northStation.leaveNeck(this);
                    }
                    break;
                // case of train nearing crossroads from north
                case CROSSROADS_N_N:
                case CROSSROADS_N_S:
                    if (trainHead == Direction.NORTH){
                        northStation.leaveNeck(this);
                    } else {
                        northStation.enterCo(this);
                    }
                default:
                    break;
            }

            // sets the speed back up
            interf.setSpeed(trainID, speed);
            System.out.println("ye boi " + trainID);
        }

    }


    public class TrackMonitor {
        private final Lock trackMonitor = new ReentrantLock(true);


        //private Queue<TrainProgram> entryQueue = new ArrayDeque<>(); // this might be where we reach trains for
                                                                     // trainstuff???

        private TrainProgram neckTrain;
        private TrainProgram primeTrain;

        private Condition neckClear = trackMonitor.newCondition();

        
        /**
         * Awaits permission and then enters neck exclusive part of monitor.
         * 
         * @param train
         * @throws InterruptedException
         */
        public void enterCo(TrainProgram train) throws InterruptedException {
            trackMonitor.lock();
            if (neckTrain != null){
               // trackMonitor.unlock();
                System.out.println("awaiting enterCo nr " + train.trainID);
                neckClear.await();
                // signal and continue so not checking condition neckTrain == null could be risky but not here since we only have two trains
               // trackMonitor.lock();
            }
            neckTrain = train;
            trackMonitor.unlock();

        }
        /**
         * Enters prime if allowed, contra fork
         * @param train
         * @return true if entered, otherwise false
         */
        public boolean requestContra(TrainProgram train){
            trackMonitor.lock();
            if (primeTrain == null){
                System.out.println("requestContra granted nr " + train.trainID);
                primeTrain = train;
                trackMonitor.unlock();
                return true;
            }
            trackMonitor.unlock();
            System.out.println("requestContra denied nr " + train.trainID);

            return false;

        }
        /**
         * Enters prime if allowed, co fork
         * @param train
         * @return
         */
        public boolean requestCo(TrainProgram train){
            trackMonitor.lock();
            if (primeTrain == null
              ||primeTrain == train){ // TODO case of leaving crossroads south direction
                primeTrain = train;
                //neckTrain = null;   // leaves neck, TODO
                neckClear.signal(); //signal to waiting trains
                trackMonitor.unlock();
                return true;
            }
            //neckTrain = null; TODO naaah
            trackMonitor.unlock();
            return false;
        }

        public void transitionContra(TrainProgram train) throws InterruptedException {
            trackMonitor.lock();
            if (neckTrain != null){
               // trackMonitor.unlock();
                neckClear.await();
               // trackMonitor.lock();
            }
            if (primeTrain == train) primeTrain = null; // leaves prime
            neckTrain = train;
            trackMonitor.unlock();
        }
        /**
         * should be called after allowed entrance to next monitor
         * If train is neither neck or prime train, nothing happens
         * @param train
         */
        public void leaveCoContra(TrainProgram train){
            trackMonitor.lock();
            if (neckTrain == train){
                neckTrain = null;
                neckClear.signal();

            }
            if (primeTrain == train){
                primeTrain = null;
            }
            trackMonitor.unlock();
        }
        /**
         * TODO this is bad
         * @param train
         */
        public void leaveNeck(TrainProgram train){
            trackMonitor.lock();
            if (neckTrain == train){
                neckTrain = null;
                neckClear.signal();
                trackMonitor.unlock();
            }
        }
    }
}

