seir_ages <- function(dias = 300,
                      duracionE, 
                      duracionIi, 
                      porc_gr, 
                      duracionIg, 
                      porc_cr, 
                      duracionIc,
                      ifr = c(.03,.03,.03),
                      vacunados = c(0,0,0),
                      contact_matrix,
                      transmission_probability,
                      N = c(1/3,1/3,1/3), 
                      zero_sus,
                      zero_exp,
                      zero_cases = c(0,1/45e6*N,0),
                      zero_rec,
                      zero_D,
                      zero_d,
                      duracion_inmunidad=180,
                      Rt=c(1.1,1.1,1.1)
){
  # cada columna es un grupo
  e = E = S = i = Ss = I = Ii = Ig = Ic = r = R = D = d = U = beta = matrix(0,dias,3)
  # zero cases
  S[1,] = zero_sus
  Ss[1,] = zero_sus
  E[1,] = zero_exp
  Ii[1,] = zero_cases
  R[1,] = zero_rec
  d[1,] = zero_d
  D[1,] = zero_D
  
  # efecto vacunas
  porc_gr = porc_gr * (1-vacunados)
  porc_cr = porc_cr * (1-vacunados)
  
  # seir
  for(t in 2:dias){
    
    # contagiados según matriz de contacto
    
    #beta       = contact_matrix * transmission_probability #r0/duracionI
    beta       = t(t(contact_matrix)/colSums(contact_matrix)) * Rt / duracionIi
    e[t-1,]    = S[t-1,] * (beta %*% I[t-1,]/N)
    e[t-1,]    =  pmin(e[t-1,], S[t-1,]) # no negativo
    
    # resto seir
    E[t,]      = E[t-1,] + e[t-1,] - E[t-1,]/duracionE
    i[t,]      = E[t-1,]/duracionE
    Ii[t,]     = Ii[t-1,] + i[t,] - Ii[t-1,]/duracionIi
    Ig[t,]     = Ig[t-1,] - Ig[t-1,]/duracionIg + Ii[t-1,]/duracionIi*porc_gr
    Ic[t,]     = Ic[t-1,] - Ic[t-1,]/duracionIc + Ii[t-1,]/duracionIi*porc_cr
    I[t,]      = Ii[t,] + Ig[t,] + Ic[t,]
    d[t,]      = Ic[t-1,]/duracionIc * ifr/porc_cr # siendo ifr = d[t]/i[t-duracionIi-duracionIc]
    D[t,]      = D[t-1,] + d[t,]
    U[t,]      = U[t-1,] + Ii[t-1,]/duracionIi*(1-porc_gr-porc_cr) + Ig[t-1,]/duracionIg + Ic[t-1,]/duracionIc * (1-ifr/porc_cr)
    R[t,]      = U[t,] + D[t,]
    
    if (t>duracion_inmunidad+1) {
      S[t,]      = N - E[t,] - I[t,] - R[t,] + (U[t-180,]-U[t-181,])
    } else {
      S[t,]      = N - E[t,] - I[t,] - R[t,]
    }
                      
    
    Ss[t,]     = Ss[t-1,] - E[t,] # ver
  }
  #browser()
  results = list(g1 = data.frame(fecha = 1:dias, S=S[,1], E=E[,1], I=I[,1], D=D[,1],R=R[,1],i=i[,1]),
                 g2 = data.frame(fecha = 1:dias, S=S[,2], E=E[,2], I=I[,2], D=D[,2],R=R[,2],i=i[,2]),
                 g3 = data.frame(fecha = 1:dias, S=S[,3], E=E[,3], I=I[,3], D=D[,3],R=R[,3],i=i[,3]))
  return(bind_rows(
    tibble(group = "g1", results[["g1"]]),
    tibble(group = "g2", results[["g2"]]),
    tibble(group = "g3", results[["g3"]]))
  )
}



# seir_ages <- function(dias,
#                       duracionE, 
#                       duracionIi, porc_gr, duracionIg, porc_cr, duracionIc,
#                       ifr = c(.03,.03,.03),
#                       vacunados = c(0,0,0),
#                       contact_matrix,
#                       transmission_probability,
#                       N = c(1/3,1/3,1/3), 
#                       zero_cases = c(0,1/45e6*N,0)){
#   
#   # cada columna es un grupo
#   e = E = S =  i = I = Ii = Ig = Ic = r = R = D = d = U = beta = matrix(0,dias,3)
#   # zero cases
#   I[1,] = zero_cases
#   S[1,] = N - zero_cases
#   # efecto vacunas
#   porc_gr = porc_gr * (1-vacunados)
#   porc_cr = porc_cr * (1-vacunados)
#   
#   # seir
#   for(t in 2:dias){
#     
#     # contagiados según matriz de contacto
#     beta       = contact_matrix * transmission_probability
#     e[t-1,]    = S[t-1,] * (beta %*% I[t-1,]/N)
#     e[t-1,]    =  pmin(e[t-1,], S[t-1,]) # no negativo
#     
#     # resto seir
#     E[t,]      = E[t-1,] + e[t-1,] - E[t-1,]/duracionE
#     i[t,]      = E[t-1,]/duracionE
#     Ii[t,]     = Ii[t-1,] + i[t,] - Ii[t-1,]/duracionIi
#     Ig[t,]     = Ig[t-1,] - Ig[t-1,]/duracionIg + Ii[t-1,]/duracionIi*porc_gr
#     Ic[t,]     = Ic[t-1,] - Ic[t-1,]/duracionIc + Ii[t-1,]/duracionIi*porc_cr
#     I[t,]      = Ii[t,] + Ig[t,] + Ic[t,]
#     d[t,]      = Ic[t-1,]/duracionIc * ifr/porc_cr # siendo ifr = d[t]/i[t-duracionIi-duracionIc]
#     D[t,]      = D[t-1,] + d[t,]
#     U[t,]      = U[t-1,] + Ii[t-1,]/duracionIi*(1-porc_gr-porc_cr) + Ig[t-1,]/duracionIg + Ic[t-1,]/duracionIc * (1-ifr/porc_cr)
#     R[t,]      = U[t,] + D[t,]
#     S[t,]      = N - E[t,] - I[t,] - R[t,]
#   }
#   results = list(g1 = data.frame(fecha = 1:dias, S=S[,1], E=E[,1], I=I[,1], D=D[,1],R=R[,1]),
#                  g2 = data.frame(fecha = 1:dias, S=S[,2], E=E[,2], I=I[,2], D=D[,2],R=R[,2]),
#                  g3 = data.frame(fecha = 1:dias, S=S[,3], E=E[,3], I=I[,3], D=D[,3],R=R[,3]))
#   return(bind_rows(
#     tibble(group = "g1", results[["g1"]]),
#     tibble(group = "g2", results[["g2"]]),
#     tibble(group = "g3", results[["g3"]]))
#   )
# }
# 
