#     trFull <- predict(dummyVars(y ~ ., data = training), newdata = training)
#     trFull <- data.frame(training$y,trFull)
#     names(trFull)[1] <- "y"
#     spl <-strsplit(split = " {1,}", "27  32  37  39  40  44  63  74  77  78  88  91  93  95  97  98 100 101 102 120 123 150 151 155 156 163 164 166 167 168 169 170 176 177 178 196 202 206 210 212 214 215 218 228 229 232 234 241 244 247 252 253 254 256 267 278 281 285 287 292 294 302 310 311 316 318 320 328 330 339 340 341 342 343 345 346 357 361 362 366 372 374 376 377 389 390 394", split = " {1,}")    
#     trFull <- trFull[,-as.integer(spl[[1]])]
#     spl <-strsplit(split = " {1,}", "26  34  35  38  56  66  68  77  79  80  81  82  83 100 102 128 131 137 138 160 165 168 171 172 173 175 184 186 187 193 195 197 201 202 222 224 227 228 232 233 247 251 252 260 261 270 280 283 286 291 292 307")    
#     trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "25  32  34  51  60  61  69  70  86  87 112 114 119 140 144 146 148 157 158 164 165 168 187 188 190 193 206 209 216 219 224 233 235 237 241 255")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "12  24  28  30  31  47  55  62  77 101 102 103 106 126 129 130 131 139 144 145 146 164 165 167 174 179 181 187 193 201 202 203 206 219")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "13  22  26  41  42  48  54  68  93 112 121 125 129 137 141 142 143 153 154 159 161 164 171 173 185")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# 
# spl <-strsplit(split = " {1,}", "20  23  42  47  60  84 102 104 110 113 122 126 135 139 142 148 149 160")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# 
# spl <-strsplit(split = " {1,}", "12  19  21  39  43  55  78  83  95 101 103 114 122 123 125 126 127 132 133 142")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "17  18  23  35  38  49  71  86  91  92  95  96 100 102 110 122")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "16  29  31  33  43  64  78  79  82  87  88  95  99 106")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "15 28 29 38 58 73 77 83 92")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# 
# spl <-strsplit(split = " {1,}", "14 26 34 53 67 70 75 83")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "13 17 24 31 49 53 62 64 68 75")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "11 12 15 21 27 44 55 56 59 65")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "10 17 22 38 48 50 55")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "9 15 19 34 43 44 48")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "8 13 16 30 38 41")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", " 7 11 13 26 29 33 35")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "6  9 10 22 27 28")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# 
# spl <-strsplit(split = " {1,}", "5  7 18 20 22")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "4  5 15 16 17")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "3 12")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "3 12")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# spl <-strsplit(split = " {1,}", "2 10")    
# trFull <- trFull[,-as.integer(spl[[1]])]
# 
# spl <-strsplit(split = " {1,}", "2")
# trFull <- trFull[,-as.integer(spl[[1]])]
# 
# 
