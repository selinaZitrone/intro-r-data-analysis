
$PARAM CL = 12, V = 100, KA = 1.2, PROP_ERR = 0.2, ADD_ERR = 0.5
$CMT GUT CENT
$OMEGA
0.04
0.04
0.04
$SIGMA
1
$MAIN
  double CLi = CL * exp(ETA(1));
  double Vi  = V  * exp(ETA(2));
  double KAi = KA * exp(ETA(3));
$ODE
  dxdt_GUT  = -KAi * GUT;
  dxdt_CENT =  KAi * GUT - (CLi/Vi) * CENT;
$TABLE
  double CP = CENT / Vi;
  double DV = CP * (1 + PROP_ERR * EPS(1)) + ADD_ERR * EPS(1);
$CAPTURE CP DV

