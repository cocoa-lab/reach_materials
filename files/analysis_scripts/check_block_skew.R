################################################################################
# This program goes through all experiment 1 test data and checks for any skew
# in the ordering of conditions, which were blocked. This is necessary since
# ordering was randomized but not counter-balanced.
################################################################################

# directory in which the test data is stored
DATA_DIR = paste(getwd(), "/data/", sep="",collapse="")

# list of test data files
# except the 1st and 7th in the directory
# sub 515 has wrong penalty column name, sub 521 has no test data
TEST_FILES = list.files(DATA_DIR, pattern = "test_output.csv$")[-c(1, 7)]

# empty dataframe for storing counts of condition order pairs
# 3 conditions, so 3x3 == 9 combinations
counts = data.frame("00"   = c(0), "0100"   = c(0), "0500"   = c(0),
                    "1000" = c(0), "100100" = c(0), "100500" = c(0),
                    "5000" = c(0), "500100" = c(0), "500500" = c(0))

# list of block numbers
BLOCKS = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

for (file in TEST_FILES) {
  
    print(file)

    # current subject's test data
    sub_data = read.csv(paste(DATA_DIR, file, sep="",collapse=""))

    # initialize variable for storing condition of the previous block
    pcond = "init"

    for (block in BLOCKS) {

        # data for the current block
        block_data = sub_data[sub_data['block'] == block,]

        # condition of current block, as a string
        ccond = toString(abs(block_data[1, "pen_val"]))

        # column for the pair of last two conditions, "X" is just an artifact of R
        col = paste("X", pcond, ccond, sep="", collapse="")
        
        print(col)

        if (block != BLOCKS[1]) {
            # increment the counter for the condition pair
            counts[col] = counts[col] + 1
        }
        # assign the current condition to the 'previous condition' variable
        pcond = ccond
    }
}

write.table(counts,
            file = paste(getwd(), "/cond_order_skew.csv", sep="",collapse=""),
            row.names = FALSE, sep=',')
