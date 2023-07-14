import org.arl.fjage.*
import org.arl.fjage.param.Parameter
import org.arl.unet.*


  class attribute extends UnetAgent {

  enum Params implements Parameter {        
    delay , info
  }

  final String title = 'Attribute request agent'        
 

  int delay = 0;   
  LinkedHashMap info = []
  int ea = 100;
  int E0=100;


  @Override
  void startup() {
    
    subscribeForService(Services.DATAGRAM);

    def phy = agentForService(Services.PHYSICAL);
  
    
    int d = 20000 + (Math.random()*20000) //20 seconds + (0-20 seconds) // 
    
     
      
      //periodically broadcast attribute requests with a random delay
      add new TickerBehavior(d, {  
          def mynode = agentForService(Services.NODE_INFO);
          //System.out.println(mynode.nodeName + ' broadcasted attribute requests');
          log.info('--' + mynode.nodeName + ' broadcasted attribute requests')
          
         phy << new ClearReq()
         phy << new DatagramReq(to: 0, protocol: Protocol.USER, data: [ mynode.location[0], mynode.location[1], mynode.location[2] ] );
      })
   
    
  
  }

  @Override
  void processMessage(Message msg) {
    if (msg instanceof DatagramNtf && msg.protocol == Protocol.USER) {
      // respond to protocol USER datagram (Attribute requests)
     
     def node = agentForService(Services.NODE_INFO);
     System.out.println(node.nodeName + ' received request from ' + msg.from);
     log.info('--' + node.nodeName + ' received request from ' + msg.from )
     
     
     //int lq = Math.random()*100; //link quality
     int lq
     float distance = Math.sqrt(Math.pow((msg.data[0]-node.location[0]),2) + Math.pow((msg.data[1]-node.location[1]),2) + Math.pow((msg.data[2]-node.location[2]),2))
     
     distance = distance / 1000
     int N0 = 1 // noise power density
     int k = 1.5 //spreading factor
     int af = 3 // absorption coefficient for frequency f = 18KHZ

     
     int d = Math.random()*5000; //delay
     
     int depth= -1*node.location[2];
     int x = node.location[0];
     int y = node.location[1];
     float dp = depth/1000

     int Eb=E0  * Math.exp(-af*dp)
     
     
     ea=ea * Math.exp(-(0.135) * (dp))
     lq = Eb / ( N0 * Math.pow( distance , k ) * Math.pow( af , distance ) ) 
    
      
        add new WakerBehavior(d, {
         log.info("The EB value :" + Eb + " Link quality:" + lq);
        log.info("The EA value :" + ea + " from node " + node.nodeName + "Sending to " + msg.from);
        String attstring = lq + ' ' + ea + ' ' + depth + ' ' + x + ' ' + y
        send new DatagramReq(
          recipient: msg.sender,
          to: msg.from,
          protocol: Protocol.DATA,
         // data: [lq , ea ,depth] //the three attributes    
        data:attstring as byte[]
        )
      })
      ///////
       String att = Integer.toString(lq) + ',' + Integer.toString(ea) + ',' + Integer.toString(depth) + ',' + Integer.toString(x) + ',' + Integer.toString(y);
       System.out.println(node.nodeName + ' replied with its attributes ' + att +  ' to ' + msg.from);
       log.info('--' + node.nodeName + ' replied with its attributes ' + att + ' to ' + msg.from);//////
       
      ///////
    }
    

    
  }

  List<Parameter> getParameterList() {      
    allOf(Params)
  }

}
