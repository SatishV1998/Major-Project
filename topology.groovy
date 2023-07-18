import org.arl.fjage.*
import org.arl.fjage.*
import org.arl.unet.*
import org.arl.unet.phy.*
import org.arl.unet.sim.*
import org.arl.unet.sim.channels.*
import static org.arl.unet.Services.*
import static org.arl.unet.phy.Physical.*


println '''
8-node network
--------------

Node 1: tcp://localhost:1101, http://localhost:8081/
Node 2: tcp://localhost:1102, http://localhost:8082/
Node 3: tcp://localhost:1103, http://localhost:8083/
Node 4: tcp://localhost:1104, http://localhost:8084/
Node 5: tcp://localhost:1105, http://localhost:8085/
Node 6: tcp://localhost:1106, http://localhost:8086/
Node 7: tcp://localhost:1107, http://localhost:8087/
Node 8: tcp://localhost:1108, http://localhost:8088/

'''

///////////////////////////////////////////////////////////////////////////////
// simulator configuration

platform = RealTimePlatform   // use real-time mode

//extra added code
import org.arl.unet.sim.channels.*

channel.model = org.arl.unet.sim.channels.ProtocolChannelModel

channel.soundSpeed = 1500.mps           // c
channel.communicationRange = 500.m     // Rc
channel.detectionRange = 2500.m         // Rd
channel.interferenceRange = 3000.m      // Ri
channel.pDetection = 1                  // pd
channel.pDecoding = 1

//end of extra added code

//energy attribute
//modem.model = MyHalfDuplexModem
//energy attribute



// run the simulation forever
simulate {
  node '1', location: [ 0.m, 0.m, 0.m], web: 8081, api: 1101, stack: "$home/etc/setup"
  node '2', location: [ 0.m,   600.m, -200.m], web: 8082, api: 1102, stack: "$home/etc/setup"
  node '3', location: [ 175.m, 500.m, -50.m], web: 8083, api: 1103, stack: "$home/etc/setup"
  node '4', location: [ 400.m, 550.m, -500.m], web: 8084, api: 1104, stack: "$home/etc/setup"
  node '5', location: [ 0.m,   300.m, -350.m], web: 8085, api: 1105, stack: "$home/etc/setup"
  node '6', location: [ 275.m, 250.m, -475.m], web: 8086, api: 1106, stack: "$home/etc/setup"
  node '7', location: [ 500.m, 335.m, -915.m], web: 8087, api: 1107, stack: "$home/etc/setup"
  node '8', location: [ 220.m, 0.m, -680.m], web: 8088, api: 1108, stack: "$home/etc/setup"
}
