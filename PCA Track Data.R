View(mtrack)
library(factoextra)


res.pca<-prcomp(mtrack, scale=TRUE)
fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind  = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969")  # Individuals color

eig.val <- get_eigenvalue(res.pca)#eignvalues
eig.val
#eigenvalue variance.percent cumulative.variance.percent
#Dim.1 6.62214613       82.7768266                    82.77683
#Dim.2 0.87761829       10.9702287                    93.74706
#Dim.3 0.15932114        1.9915143                    95.73857
#Dim.4 0.12404939        1.5506173                    97.28919
#Dim.5 0.07988027        0.9985034                    98.28769
#Dim.6 0.06796515        0.8495644                    99.13725
#Dim.7 0.04641953        0.5802441                    99.71750
#Dim.8 0.02260010        0.2825012                   100.00000

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
#Dim.1       Dim.2       Dim.3       Dim.4       Dim.5        Dim.6
#m100     0.8171850  0.53105812  0.13262256 -0.04495150  0.07420631 -0.154779432
#m200     0.8671665  0.43245706  0.14395631  0.09126224 -0.04351305  0.171055680
#m400     0.9152012  0.23258563 -0.22371086 -0.22975863 -0.06170481  0.040832400
#m800     0.9487545  0.01164452 -0.21254058  0.16905873  0.15263566 -0.003830171
#m1500    0.9593714 -0.13096330 -0.06124674  0.14247117 -0.13784339 -0.041149873
#K5       0.9376633 -0.29231413  0.07574451 -0.01042093 -0.07178238 -0.036836745
#K10      0.9438353 -0.28747025  0.07254627 -0.02820068 -0.03763977 -0.057097946
#Marathon 0.8798965 -0.41122586  0.10505987 -0.10549010  0.14072995  0.082195160
#Dim.7         Dim.8
#m100      0.0293534410 -1.586643e-02
#m200     -0.0242684024  1.444016e-02
#m400     -0.0006148367  1.912287e-05
#m800     -0.0512810242  5.737487e-03
#m1500     0.1314281444 -2.094007e-02
#K5       -0.1273964719 -8.218675e-02
#K10      -0.0381072008  1.197848e-01
#Marathon  0.0859270102 -2.377729e-02
res.var$contrib        # Contributions to the PCs
#Dim.1       Dim.2     Dim.3       Dim.4     Dim.5      Dim.6        Dim.7
#m100     10.08421 32.13501032 11.039805  1.62889747  6.893537 35.2484651 1.856168e+00
#m200     11.35550 21.30984610 13.007325  6.71409763  2.370280 43.0515422 1.268766e+00
#m400     12.64836  6.16396374 31.412372 42.55484863  4.766488  2.4531468 8.143645e-04
#m800     13.59280  0.01545031 28.353737 23.03989878 29.165705  0.0215849 5.665166e+00
#m1500    13.89872  1.95431039  2.354467 16.36286548 23.786598  2.4914416 3.721140e+01
#K5       13.27685  9.73630027  3.601048  0.08754232  6.450542  1.9965317 3.496343e+01
#K10      13.45221  9.41629670  3.303366  0.64109834  1.773595  4.7968340 3.128336e+00
#Marathon 11.69134 19.26882217  6.927880  8.97075134 24.793254  9.9404537 1.590592e+01
#Dim.8
#m100     1.113905e+00
#m200     9.226433e-01
#m400     1.618064e-06
#m800     1.456576e-01
#m1500    1.940198e+00
#K5       2.988775e+01
#K10      6.348826e+01
#Marathon 2.501581e+00
res.var$cos2           # Quality of representation 
#Dim.1        Dim.2       Dim.3        Dim.4       Dim.5        Dim.6
#m100     0.6677913 0.2820227294 0.017588743 0.0020206373 0.005506576 2.395667e-02
#m200     0.7519778 0.1870191078 0.020723419 0.0083287969 0.001893386 2.926005e-02
#m400     0.8375932 0.0540960735 0.050046549 0.0527890282 0.003807484 1.667285e-03
#m800     0.9001351 0.0001355948 0.045173497 0.0285808529 0.023297645 1.467021e-05
#m1500    0.9203935 0.0171513855 0.003751163 0.0202980341 0.019000799 1.693312e-03
#K5       0.8792124 0.0854475524 0.005737231 0.0001085957 0.005152710 1.356946e-03
#K10      0.8908251 0.0826391424 0.005262961 0.0007952785 0.001416752 3.260175e-03
#Marathon 0.7742178 0.1691067084 0.011037577 0.0111281619 0.019804919 6.756044e-03
#Dim.7        Dim.8
#m100     8.616245e-04 2.517435e-04
#m200     5.889554e-04 2.085183e-04
#m400     3.780242e-07 3.656842e-10
#m800     2.629743e-03 3.291876e-05
#m1500    1.727336e-02 4.384866e-04
#K5       1.622986e-02 6.754661e-03
#K10      1.452159e-03 1.434841e-02
#Marathon 7.383451e-03 5.653597e-04

# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation #Ran, 
#but too big to paste results here
