import org.arl.fjage.*
import org.arl.fjage.param.Parameter
import org.arl.unet.*


  class attribute extends UnetAgent {

  enum Params implements Parameter {        
    delay , info
  }

  final String title = 'Attribute request agent'        
  final String description = 'Receives attribute requests from other nodes and responds with its attributes, Also updates neighbour attribute values' 

  int delay = 0;   
  LinkedHashMap info = []
  
  
    
  
  @Override
  void startup() {
    
    subscribeForService(Services.DATAGRAM);
    def phy = agentForService(Services.PHYSICAL);
  
    
    int d = 20000 + (Math.random()*20000) //20 seconds + (0-20 seconds) // 
    
     
      
      //periodically broadcast attribute requests with a random delay
      add new TickerBehavior(d, {  
          def mynode = agentForService(Services.NODE_INFO);
         // System.out.println(mynode.nodeName + ' broadcasted attribute requests');
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
     //System.out.println(node.nodeName + ' received request from ' + msg.from);
     log.info('--' + node.nodeName + ' received request from ' + msg.from )
     
     
     //int lq = Math.random()*100; //link quality
     float distance = Math.sqrt(Math.pow((msg.data[0]-node.location[0]),2) + Math.pow((msg.data[1]-node.location[1]),2) + Math.pow((msg.data[2]-node.location[2]),2))
     
     distance = distance / 1000
     
     int Eb = 40 //avg transmission energy per bit
     int N0 = 1 // noise power density
     int k = 2 //spreading factor
     int af = 3 // absorption coefficient for frequency f = 18KHZ
     int lq = Eb / ( N0 * Math.pow( distance , k ) * Math.pow( af , distance ) ) 
  
    
    
     
     
     int ea = Math.random()*100 + 1 ; // energy attribute
     int d = Math.random()*5000; //delay
     
     int depth= -1*node.location[2];
     int x = node.location[0];
     int y = node.location[1];
     
     String attstring = lq + ' ' + ea + ' ' + depth + ' ' + x + ' ' + y ;
      
        add new WakerBehavior(d, {
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
       //System.out.println(node.nodeName + ' replied with its attributes ' + att +  ' to ' + msg.from);
       log.info('--' + node.nodeName + ' replied with its attributes ' + att + ' to ' + msg.from);//////
       
      ///////
    }
    

    
  }

  List<Parameter> getParameterList() {      
    allOf(Params)
  }

}