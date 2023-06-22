import org.arl.fjage.*
import org.arl.fjage.Message
import org.arl.fjage.param.Parameter
import org.arl.unet.*
import org.arl.unet.phy.*
import java.text.SimpleDateFormat

class ClusteringAgent extends UnetAgent {
    // data members:
    def clusterList = []

    enum Params implements Parameter {
      //declare any parameters here
      nodeval,
      order
    }
    enum Param implements Parameter {        
        delay
    }


   
    static int flag = 0 //initial 
    static int  packetId = 0


    int delay = Math.abs(new Random().nextInt() % 5000) + 1000
    def averageDistance

    int r  = 1000 // Range of the node
    int d = r  / 2 
    //assuming the average distance between src node and current node to be half the range
    double T_t = 0.2  // Transmission delay assumed to be 0.2 s
    double T_proc = 0.1  // Procesing Delay assumed to be 0.1 s
    int v = 1500 //speed of sound in water
    double T_r =  (r - d) / v
    int n = 2 // priority of the node


    double hcalc(int n, double T_t, double T_proc) {
    double temp = 0
    for (int p = 1; p <= n; p++) {
        temp += ((p - 1) * T_t) + p * T_proc
    }
    return temp
   }

    LinkedHashMap nodeval = [];
    LinkedHashMap order = []; //nodes sorted according to TOPSIS 

    def topsisalg(info, Nf) {
      println('Applying TOPSIS on : ' + info)
      LinkedHashMap scores = []

      if (info.size() == 1) //only one alternative
      {
        for (entry in info) {
          scores[entry.key] = 1.0
        }

        return scores
      }

      def fsum = new float[Nf];
      def entropy = new float[Nf];
      def weights = new float[Nf];
      def best = new float[Nf];
      def worst = new float[Nf];

      //STEP -1 : To normalise the columns

      //1.1 Store the sum of each column in fsum

      for (entry in info) {
        for (int i = 0; i < Nf; i++) {
          fsum[i] = fsum[i] + entry.value[i]
        }
      }

      //println( 'nodeval after step 1: summing topsis: ' + nodeval )

      //1.2 Divide the values in each column with their respective sum
      for (entry in info) {
        for (int i = 0; i < Nf; i++) {

          //System.out.println( 'attribute value =' + (float)info[entry.key][i] + '/' + ' fsum[i] = ' + fsum[i] )

          info[entry.key][i] = ((float) info[entry.key][i]) / fsum[i]

          //System.out.println( 'norm value' + info[entry.key][i] )

        }
      }

      //System.out.println(Arrays.toString(fsum))

      // Step-2 To calculate weights using entropy formula

      //2.1 Find entropy using entropy formula

      for (int i = 0; i < Nf; i++) {
        for (entry in info) {
          entropy[i] = entropy[i] + entry.value[i] * Math.log(entry.value[i])
        }
        entropy[i] = (-1 * entropy[i]) / Math.log(info.size())
      }

      //System.out.println('entropy : ' + Arrays.toString(entropy))

      // 2.2  Calculate Weights from the respective entropies 

      for (int j = 0; j < Nf; j++) {
        weights[j] = (1 - entropy[j]) / (Nf - entropy.sum())
      }

      // Step-2 ends

      // 1. System.out.println('Weights : ' + Arrays.toString(weights))

      // Step-3 : Mutiply the attribute values with their respective weights

      for (entry in info) {
        for (int i = 0; i < Nf; i++) {
          info[entry.key][i] = info[entry.key][i] * weights[i]
        }
      }

      //Step-3 ends

      // Step-4 : Find the best and the worst alternative

      for (int i = 0; i < Nf; i++) {
        best[i] = -2
        worst[i] = 2
        for (entry in info) {
          if (entry.value[i] > best[i])
            best[i] = entry.value[i]
          if (entry.value[i] < worst[i])
            worst[i] = entry.value[i]
        }
      }

      //Step-4 Ends

      float dfb = 0
      float dfw = 0

      //Step-5 : Calculating TOPSIS score
      // 5.1 : Calculate the Distance from the Best and the Worst
      // 5.2  : Find TOPSIS score
      for (entry in info) {
        dfb = 0
        dfw = 0
        for (int i = 0; i < Nf; i++) {
          dfb = dfb + (entry.value[i] - best[i]) * (entry.value[i] - best[i])
          dfw = dfw + (entry.value[i] - worst[i]) * (entry.value[i] - worst[i])
        }
        dfb = Math.sqrt(dfb)
        dfw = Math.sqrt(dfw)
        scores[entry.key] = dfw / (dfw + dfb)
      }

      // Step-6 : Sort the alternatives according to their TOPSIS score
      scores = scores.sort {
        a,
        b -> b.value <=> a.value
      }

      return scores;
    }


//************* Cluster Formation ****************/

    // findClusterHeadID:
    int findClusterHeadID(LinkedHashMap scores, LinkedHashMap visited){
        int clusterHeadID = -1
        float max_so_far = 0


        //max score calculation and the one with max score is choosen as cluster head
        for(entry in scores){
            if(visited[entry.key] ) continue
            
            if(entry.value > max_so_far){
                max_so_far = entry.value;
                clusterHeadID = entry.key
            }
        }
        return clusterHeadID
    }
  
  //distance calculation
  float findDistance(int ID1, int ID2, LinkedHashMap coordinates) {
    float[] clusterHeadCoords = coordinates[ID1];
    float[] coords = coordinates[ID2];
    double dist_x = Math.abs(coords[3] - clusterHeadCoords[3]);
    double dist_y = Math.abs(coords[4] - clusterHeadCoords[4]);
    double dist_z = Math.abs(coords[2] - clusterHeadCoords[2]);
    return (float) Math.sqrt(dist_x*dist_x + dist_y*dist_y + dist_z*dist_z);
}



 



    // key   value
    // node  distance
    LinkedHashMap distFromClusterHead(LinkedHashMap scores, LinkedHashMap coordinates, LinkedHashMap visited){
        LinkedHashMap dist = []
        def distanceList = []
        int clusterID = findClusterHeadID(scores, visited)
        

        println("The ClusterID:" + clusterID + " ,coordinates of clusterID:" + coordinates[clusterID])
        println ("coordinates" + coordinates)
        println ("scores" + scores)
        
        for(entry in scores){
            if(visited[entry.key]) continue
            dist[entry.key] = findDistance(clusterID, entry.key, coordinates)
            distanceList.add(dist[entry.key])
            averageDistance = distanceList.sum() / distanceList.size()
        }
        
        return dist
    } 





def findCluster(scores, coordinates, visited) {
    def finalCluster = []
    int clusterID = findClusterHeadID(scores, visited)
    if (clusterID == -1) return finalCluster
    
    int range = 1000
    finalCluster << clusterID
    LinkedHashMap dist = distFromClusterHead(scores, coordinates, visited)
    visited[clusterID] = true
    def threhold_max=1.5*averageDistance

    //a threshold of 1.5 times the average distance as the maximum distance

    println('Threshold Value :' + threhold_max ) 
    dist.each { entry ->
        if (entry.key != clusterID && entry.value < range / 2 && !visited[entry.key] && dist[entry.key]<=threhold_max) {
          println('the Distance is Less than Range :'+entry.value + 'less than threshold')
            finalCluster << entry.key
        }
    }
    
    return finalCluster
}

   

    @Override
    void startup() {

      subscribeForService(Services.DATAGRAM);
      subscribeForService(Services.PHYSICAL);
      def mynode = agentForService(Services.NODE_INFO);


      add new TickerBehavior(60000, { // topsis function is called every four minute

        LinkedHashMap info = [];

        for (entry in nodeval) {
          info[entry.key] = [entry.value[0], entry.value[1], entry.value[2]]
        }

        order = topsisalg(info, 3) //function call for TOPSIS

        println('Nodes Sorted according to their TOPSIS Score:' + order)

        for (entry in order) {
           System.out.println(Integer.toString(entry.key) + ' : ' + Double.toString(entry.value));
        }

        // input for implementation part start:
        LinkedHashMap coordinates = nodeval; 
        LinkedHashMap scores = order;
        LinkedHashMap visited = [];
        
 
        println('Clustering Agent can access coordinates :' + coordinates ) 
        println('Clustering Agent can access scores :' + scores ) 
        

        
        println('Coordinates before removing: ' + coordinates)
        LinkedHashMap dummyCoordinates = []
        int senderNodeID = mynode.nodeName
        
        // abs value should be less than Sender Node

        //depth less than range
        for(entry in coordinates){
            // entry.value[2] is depth or z
            if(Math.abs(entry.value[2]) < Math.abs(Math.round(mynode.location[2]))){
                dummyCoordinates.put(entry.key, entry.value)
            }
        }
        
        coordinates = dummyCoordinates
        println('depth of Sender Node: ' + Math.abs(Math.round(mynode.location[2])))
        println('Coordinates after removing: ' + coordinates)
        
        LinkedHashMap dummyScores = []
        
        for(entry in scores){
            for(a in coordinates){
                if(a.key == entry.key){
                    dummyScores.put(entry.key, entry.value)
                }
            }
        }

        scores = dummyScores
        
        //as visited is true so it changed all to false
        for (entry in scores) {
            visited[entry.key] = false
        }
        
        // specify the threhold as it is 3 bcz it depends on 3 parameters
        int threshold = 3
        
        if(scores.size() < threshold){
            def cluster = []
            for (entry in scores){
                cluster.add(entry.key)
            }
            println(mynode.nodeName + " has Final Cluster: " + cluster)
            return
        }
        
        
        def cluster = findCluster(scores, coordinates, visited)


        clusterList.add(cluster)
        
        int totalNodesInClusterList = cluster.size()
        
        while (totalNodesInClusterList < threshold) {
            
            for (x in cluster) {
                visited[x] = true
            }
            
            def newCluster = findCluster(scores, coordinates, visited)

            clusterList.add(newCluster)
            
            cluster = newCluster
            
            totalNodesInClusterList = totalNodesInClusterList + cluster.size()
        }
        

        println(mynode.nodeName + " has Final Cluster List: " + clusterList)
        
      }) 

    }

    @Override
    void processMessage(Message msg) {

      def node = agentForService(Services.NODE_INFO);

      if (msg instanceof DatagramNtf && msg.protocol == Protocol.DATA) //Received attributes from other nodes 
      {
        String str = new String(msg.data);
        String[] strarray;
        strarray = str.split(' ');

        println(node.nodeName + ' received attributes ' + strarray + ' from ' + msg.from)

        println('nodeval before updation' + nodeval)

        nodeval[msg.from] = [Float.valueOf(strarray[0]), Float.valueOf(strarray[1]), Float.valueOf(strarray[2]), Float.valueOf(strarray[3]), Float.valueOf(strarray[4])]

        log.info('Updated neighbor attributes at ' + node.nodeName);

        println('nodeval after updation' + nodeval)
      }


        
      //def madd=node.address
      def nodes = clusterList
              
      
      if (msg instanceof DatagramNtf &&  msg.protocol == Protocol.USER  &&  !clusterList.empty) {
        def ndate = new Date()
        int l = msg.data.length
        def data_given = new int[l+1]
        def ndata = new int[l]

        ndata = msg.data.clone()
      //flag=1 means drop the packet as it already forwarded by another node
      if (flag == 1 && packetId == msg.data[0]) {
        ndate = new Date()
        println "$ndate -ht-$packetId-$node.nodeName- $node.nodeName Dropping Packet as Packet already forwarded."
        packetId = 0
        flag = 0
        return
       }

      //compare the address and see it that means to that node or not
      //if it meant for that node then n will have some value
      int n = 0
      for (int i = 0; i < l; i++) {
        if (node.address ==  ndata[i]) {
            n = i
            break
        } else if (data_given[i] == 0) {
            break
        }
    }
    
    if (n == 0) {
        ndate = new Date()
        println "$ndate -ht-$packetId-$node.nodeName- The packet is not meant for this node $node.nodeName. Dropping Packet!!!"
        return
    }
    
    //if still n is not zero and flag not changed the packetId should be the msg data
    if (flag == 0) {
        flag = 1
        packetId = msg.data[0]  
    } 
     //if flag 1 then its already forwarded
    else if (flag == 1 && packetId == msg.data[0]) {
        ndate = new Date()
        println "$ndate -ht-$packetId-$node.nodeName- $node.nodeName Dropping Packet as Packet already forwarded."
        packetId = 0
        flag = 0
        return
    }
    
    ndate = new Date()
    if (node.address == 1) { // Sink node 
        println "$ndate -ht- $packetId - $node.nodeName - $node.nodeName Sink Node Packet Recieved"
        packetId = 0
        flag = 0
        return
    }
    
    ndate = new Date()
    println "$ndate - ht - $packetId - $node.nodeName - $node.nodeName Packet recieved and checking data: $msg.data length: $l"
    
    double htime = (T_r + hcalc(n, T_t, T_proc)) * 1000
    
    println "$ndate - ht - $packetId-$node.nodeName - The current holding time is $htime ms"
    
    int hold_time = htime * 10
    data_given[0] = msg.data[0]
    
    println "Calculation oF HT"
    //transfer the data from nodes to data_given
    for (int i = 1; i < l; i++) {
        data_given[i] = nodes[i - 1]
    }
    
    println "$ndate -ht-$packetId-$node.nodeName- The data and new cluster nodes: $data_given"
    
    if (packetId == 0)
        return
    
    add new WakerBehavior(hold_time, {
          if(packetId!=0){
              send new DatagramReq(recipient: msg.sender, to: 0, protocol: Protocol.USER, data: data_given)
              ndate = new Date()
              println "$ndate -ht-$packetId-$node.nodeName- Packet Forwarding"
              packetId=0
              flag=0
        }
    })
}

    }

    List < Parameter > getParameterList() {
      allOf(Params)
      allof(Param)
    }
  }
