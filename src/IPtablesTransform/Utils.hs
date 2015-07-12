{- Author: Stefan Selent -}
module IPtablesTransform.Utils
where

import Data.Word
import Debug.Trace

doDebug = True
debug a b = if doDebug then trace b a else a

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

{-
deprecated version
-}
toDict :: [a] -> [(a,a)]
toDict [] = []
toDict [x] = []
toDict (k:v:rest) = ((k,v):xs)
    where   xs = toDict rest

{-
Takes a list of words and transforms it in a dictionary in the form of
(option, [!]value)
-}
-- negation looks like: "! -p 45"
toDict' :: [String] -> [(String,String)]
toDict' [] = []
toDict' ("!":os:val:rest) = (os, '!':val) : (toDict' rest)
toDict' (('-':os):val:rest) = (('-':os), val) : (toDict' rest)
toDict' (x:xs) = toDict' xs

{- Interval helpers -}
isIntervalNegated :: String -> Bool
isIntervalNegated ('!':' ':_) = True
isIntervalNegated _ = False



{- Range check helpers-}
checkIf2ByteSize :: Word32 -> Bool
checkIf2ByteSize portNbr = (portNbr >= 0 && portNbr < 2^16)

checkIfByteSize :: Word32 -> Bool
checkIfByteSize nbr = (nbr >= 0 && nbr < 2^8)


{- miscellaneous-}
replaceNth :: Int -> a -> [a] -> [a] 
replaceNth n newVal xs
    | length xs == 0 = []
    | n < 0 || n >= length xs = xs
    | n == 0 = newVal:(tail xs)
    | otherwise = (head xs):replaceNth (n-1) newVal (tail xs)

insertAt :: [a] -> Int -> Int -> [a] -> [a]
insertAt [] _ _ _ = []
insertAt xs low high ys = take low xs ++ ys ++ drop (high + 1) xs

protocolNameToNum :: [(String, Word32)]
protocolNameToNum = [   ("ICMP", 1)
                    ,   ("icmp", 1)
                    ,   ("IGMP", 2)
                    ,   ("igmp", 2)
                    ,   ("GGP", 3)
                    ,   ("ggp", 3)
                    ,   ("TCP", 6)
                    ,   ("tcp", 6)
                    ,   ("UDP", 17)
                    ,   ("udp", 17)
                    ]

intSqrt :: Int -> Int
intSqrt = floor . sqrt . fromIntegral

asWord y = fromIntegral y :: Word32
asInt y = fromIntegral y :: Int

splitIn3 :: (Show a) => [a] -> ([a], [a], [a])
splitIn3 [] = ([], [], [])
splitIn3 [x] = ([], [x], [])
splitIn3 xs = let (s, m:e) = splitAt ((length xs) `div` 2) xs in (s, [m], e)

trd :: (a,b,c) -> c
trd (_, _, x) = x

frth :: (a,b,c,d) -> d
frth (_,_,_,x) = x

{-
4   IPv4    IPv4 (encapsulation)
5   ST  Internet Stream Protocol    RFC 1190,
6   TCP     Transmission Control Protocol
7   CBT     Core-based trees
8   EGP     Exterior Gateway Protocol
9   IGP     Interior Gateway Protocol (any private interior gateway (used by Cisco for their IGRP))     
10  BBN-RCC-MON     BBN RCC Monitoring  
11  NVP-II  Network Voice Protocol
12  PUP     Xerox PUP   
13  ARGUS   ARGUS   
14  EMCON   EMCON   
15  XNET    Cross Net Debugger
16  CHAOS   Chaos   
17  UDP     User Datagram Protocol
18  MUX     Multiplexing
19  DCN-MEAS    DCN Measurement Subsystems  
20  HMP     Host Monitoring Protocol
21  PRM     Packet Radio Measurement    
22  XNS-IDP     XEROX NS IDP    
23  TRUNK-1     Trunk-1     
24  TRUNK-2     Trunk-2     
25  LEAF-1  Leaf-1  
26  LEAF-2  Leaf-2  
27  RDP     Reliable Datagram Protocol
28  IRTP    Internet Reliable Transaction Protocol
29  ISO-TP4     ISO Transport Protocol Class 4
30  NETBLT  Bulk Data Transfer Protocol
31  MFE-NSP     MFE Network Services Protocol   
32  MERIT-INP   MERIT Internodal Protocol   
33  DCCP    Datagram Congestion Control Protocol
34  3PC     Third Party Connect Protocol    
35  IDPR    Inter-Domain Policy Routing Protocol
36  XTP     Xpress Transport Protocol   
37  DDP     Datagram Delivery Protocol  
38  IDPR-CMTP   IDPR Control Message Transport Protocol     
39  TP++    TP++ Transport Protocol     
40  IL  IL Transport Protocol   
41  IPv6    IPv6 Encapsulation
42  SDRP    Source Demand Routing Protocol
43  IPv6-Route  Routing Header for IPv6
44  IPv6-Frag   Fragment Header for IPv6
45  IDRP    Inter-Domain Routing Protocol   
46  RSVP    Resource Reservation Protocol
47  GRE     Generic Routing Encapsulation   RFC 2784,
48  MHRP    Mobile Host Routing Protocol    
49  BNA     BNA     
50  ESP     Encapsulating Security Payload
51  AH  Authentication Header
52  I-NLSP  Integrated Net Layer Security Protocol  TUBA
53  SWIPE   SwIPe   IP with Encryption
54  NARP    NBMA Address Resolution Protocol
55  MOBILE  IP Mobility (Min Encap)
56  TLSP    Transport Layer Security Protocol (using Kryptonet key management)  
57  SKIP    Simple Key-Management for Internet Protocol
58  IPv6-ICMP   ICMP for IPv6   RFC 4443,
59  IPv6-NoNxt  No Next Header for IPv6
60  IPv6-Opts   Destination Options for IPv6
61  Any host internal protocol  
62  CFTP    CFTP    
63  Any local network   
64  SAT-EXPAK   SATNET and Backroom EXPAK   
65  KRYPTOLAN   Kryptolan   
66  RVD     MIT Remote Virtual Disk Protocol    
67  IPPC    Internet Pluribus Packet Core   
68  Any distributed file system     
69  SAT-MON     SATNET Monitoring   
70  VISA    VISA Protocol   
71  IPCV    Internet Packet Core Utility    
72  CPNX    Computer Protocol Network Executive     
73  CPHB    Computer Protocol Heart Beat    
74  WSN     Wang Span Network   
75  PVP     Packet Video Protocol   
76  BR-SAT-MON  Backroom SATNET Monitoring  
77  SUN-ND  SUN ND PROTOCOL-Temporary   
78  WB-MON  WIDEBAND Monitoring     
79  WB-EXPAK    WIDEBAND EXPAK  
80  ISO-IP  International Organization for Standardization Internet Protocol    
81  VMTP    Versatile Message Transaction Protocol
82  SECURE-VMTP     Secure Versatile Message Transaction Protocol
83  VINES   VINES   
84  TTP     TTP     
84  IPTM    Internet Protocol Traffic Manager   
85  NSFNET-IGP  NSFNET-IGP  
86  DGP     Dissimilar Gateway Protocol     
87  TCF     TCF     
88  EIGRP   EIGRP   
89  OSPF    Open Shortest Path First
90  Sprite-RPC  Sprite RPC Protocol     
91  LARP    Locus Address Resolution Protocol   
92  MTP     Multicast Transport Protocol    
93  AX.25   AX.25   
94  IPIP    IP-within-IP Encapsulation Protocol
95  MICP    Mobile Internetworking Control Protocol     
96  SCC-SP  Semaphore Communications Sec. Pro   
97  ETHERIP     Ethernet-within-IP Encapsulation
98  ENCAP   Encapsulation Header
99  Any private encryption scheme   
100     GMTP    GMTP    
101     IFMP    Ipsilon Flow Management Protocol    
102     PNNI    PNNI over IP    
103     PIM     Protocol Independent Multicast  
104     ARIS    IBM's ARIS (Aggregate Route IP Switching) Protocol  
105     SCPS    SCPS (Space Communications Protocol Standards)  SCPS-TP[1]
106     QNX     QNX     
107     A/N     Active Networks     
108     IPComp  IP Payload Compression Protocol
109     SNP     Sitara Networks Protocol    
110     Compaq-Peer     Compaq Peer Protocol    
111     IPX-in-IP   IPX in IP   
112     VRRP    Virtual Router Redundancy Protocol, Common Address Redundancy Protocol (not IANA assigned)  VRRP:RFC 3768
113     PGM     PGM Reliable Transport Protocol
114     Any 0-hop protocol  
115     L2TP    Layer Two Tunneling Protocol Version 3
116     DDX     D-II Data Exchange (DDX)    
117     IATP    Interactive Agent Transfer Protocol     
118     STP     Schedule Transfer Protocol  
119     SRP     SpectraLink Radio Protocol  
120     UTI     Universal Transport Interface Protocol  
121     SMP     Simple Message Protocol     
122     SM  Simple Multicast Protocol   draft-perlman-simple-multicast-03
123     PTP     Performance Transparency Protocol   
124     IS-IS over IPv4     Intermediate System to Intermediate System (IS-IS) Protocol over IPv4   RFC 1142 and
125     FIRE    Flexible Intra-AS Routing Environment   
126     CRTP    Combat Radio Transport Protocol     
127     CRUDP   Combat Radio User Datagram  
128     SSCOPMCE    Service-Specific Connection-Oriented Protocol in a Multilink and Connectionless Environment     ITU-T Q.2111 (1999)
129     IPLT        
130     SPS     Secure Packet Shield    
131     PIPE    Private IP Encapsulation within IP  Expired I-D draft-petri-mobileip-pipe-00.txt
132     SCTP    Stream Control Transmission Protocol    
133     FC  Fibre Channel   
134     RSVP-E2E-IGNORE     Reservation Protocol (RSVP) End-to-End Ignore
135     Mobility Header     Mobility Extension Header for IPv6
136     UDPLite     Lightweight User Datagram Protocol
137     MPLS-in-IP  Multiprotocol Label Switching Encapsulated in IP
138     manet   MANET Protocols
139     HIP     Host Identity Protocol
140     Shim6   Site Multihoming by IPv6 Intermediation
141     WESP    Wrapped Encapsulating Security Payload
142     ROHC
                    
-}

