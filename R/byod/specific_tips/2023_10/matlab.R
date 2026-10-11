library(R.matlab)
x <- R.matlab::readMat("http://ufldl.stanford.edu/housenumbers/test_32x32.mat")

data_paula <- R.matlab::readMat("data/byod/Participant1_Behavioral_Data.mat")

data_paula[3]
