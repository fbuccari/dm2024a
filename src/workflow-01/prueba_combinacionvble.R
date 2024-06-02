require("data.table")
require("yaml")
#install.packages("randomForest")
#install.packages("proxy")  # Para calcular las distancias a partir de las proximidades
library(randomForest)
library(proxy)

dataset <- fread("~/buckets/b1/datasets/competencia_2024.csv.gz")

dataset[, kmes := foto_mes %% 100]

# creo un ctr_quarter que tenga en cuenta cuando
# los clientes hace 3 menos meses que estan
# ya que seria injusto considerar las transacciones medidas en menor tiempo
dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]
dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]
dataset[
  cliente_antiguedad == 3,
  ctrx_quarter_normalizado := ctrx_quarter * 1.2
]

# variable extraida de una tesis de maestria de Irlanda
dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

# se crean los nuevos campos para MasterCard  y Visa,
#  teniendo en cuenta los NA's
# varias formas de combinar Visa_status y Master_status
dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
dataset[, vm_status02 := Master_status + Visa_status]

dataset[, vm_status03 := pmax(
  ifelse(is.na(Master_status), 10, Master_status),
  ifelse(is.na(Visa_status), 10, Visa_status)
)]

dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
        + ifelse(is.na(Visa_status), 10, Visa_status)]

dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
        + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

dataset[, vm_status06 := ifelse(is.na(Visa_status),
                                ifelse(is.na(Master_status), 10, Master_status),
                                Visa_status
)]

dataset[, mv_status07 := ifelse(is.na(Master_status),
                                ifelse(is.na(Visa_status), 10, Visa_status),
                                Master_status
)]


# combino MasterCard y Visa
dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

# a partir de aqui juego con la suma de Mastercard y Visa
dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]
dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]
dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]
dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]
dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]
dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]
dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]
dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]
dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]
dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]
dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]
dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]
dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]
dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]
dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]
dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

# Aqui debe usted agregar sus propias nuevas variables
dataset[,rent_anual_edad:= mrentabilidad_annual / (cliente_edad)^2]
dataset[,comisiones_edad:= mcomisiones / (cliente_edad)^2]
dataset[,activos_margen_edad:= mactivos_margen / (cliente_edad)^2]
dataset[,pasivos_margen_edad:= mpasivos_margen / (cliente_edad)^2]
dataset[,productos_edad:= cproductos / cliente_edad]
dataset[,productos_antiguedad:= cproductos / cliente_antiguedad]
dataset[,edad_cuentas_productos:= cliente_edad / (tcuentas+cproductos)]
dataset[,cliente_cuentas_productos:= cliente_antiguedad / (tcuentas+cproductos)]
dataset[,rentabilidad_productos_cuentas:= mrentabilidad / (tcuentas+cproductos)]
dataset[,rent_anual_prod_cuentas:= mrentabilidad_annual / (tcuentas+cproductos)]
dataset[,edad_cuentas_prod_cc:= cliente_edad / (tcuentas+cproductos+ccuenta_corriente)]
dataset[,antig_cuentas_prod_cc:= cliente_antiguedad / (tcuentas+cproductos+ccuenta_corriente)]
dataset[,rentabilidad_cuentas_prod:= mrentabilidad / (tcuentas+cproductos+ccuenta_corriente)]
dataset[,rent_anual_cuentas_prod:= mrentabilidad_annual / (tcuentas+cproductos+ccuenta_corriente)]
dataset[,renta_montocc:= mrentabilidad / mcuenta_corriente]
dataset[,renta_a_montocc:= mrentabilidad_annual / mcuenta_corriente]
dataset[,renta_monto_ca:= mrentabilidad / (mcaja_ahorro+mcaja_ahorro_adicional+mcaja_ahorro_dolares)]
#dataset[,renta_a_monto_ca:= mrentabilidad_annual / rowSums(cbind(mcaja_ahorro,mcaja_ahorro_adicional,mcaja_ahorro_dolares),na.rm=TRUE)]
dataset[,comisiones_monto_ca:= mcomisiones / (mcaja_ahorro+mcaja_ahorro_adicional+mcaja_ahorro_dolares)]
dataset[,pasivos_margen_ca:= mpasivos_margen / (mcaja_ahorro+mcaja_ahorro_adicional+mcaja_ahorro_dolares)]
dataset[,rentabilidad_saldo:= mrentabilidad / mcuentas_saldo]
dataset[,renta_a_saldo:= mrentabilidad_annual / mcuentas_saldo]
dataset[,comisiones_cuenta_saldo:= mcomisiones / mcuentas_saldo]
dataset[,pasivos_cuenta_saldo:= mpasivos_margen / mcuentas_saldo]
dataset[,renta_transacciones_debito:= mrentabilidad / ctarjeta_debito_transacciones]
dataset[,renta_anual_tarjeta:= mrentabilidad_annual / ctarjeta_debito_transacciones]
dataset[,renta_autoservicio:= mrentabilidad / mautoservicio]
dataset[,renta_anual_autoservicio:= mrentabilidad_annual / mautoservicio]
#dataset[,total_consumo:= rowSums(cbind(mautoservicio,mtarjeta_visa_consumo,mtarjeta_master_consumo),na.rm=TRUE)]
dataset[,consumo_sobre_limite_visa:= mtarjeta_visa_consumo / Visa_mlimitecompra]
dataset[,consumo_sobre_limite_master:= mtarjeta_visa_consumo / Master_mlimitecompra]
dataset[,inversiones_edad:= (minversion1_pesos+minversion1_dolares+minversion2+mplazo_fijo_dolares+mplazo_fijo_pesos) / (cliente_edad)^2]
dataset[,inversiones_sueldo:= (minversion1_pesos+minversion1_dolares+minversion2) / mpayroll]
dataset[,inversiones_sueldo_total:= (minversion1_pesos + minversion1_dolares + minversion2) / (mpayroll + mpayroll2)]
dataset[,sueldo_total_edad:= (mpayroll + mpayroll2) / cliente_edad]
#dataset[,total_debitos_automaticos:= (mcuenta_debitos_automaticos+mtarjeta_visa_debitos_automaticos+mttarjeta_master_debitos_automaticos)]
dataset[,descuentos_sobre_consumos:= (mtarjeta_visa_descuentos/mtarjeta_visa_consumo) + (mtarjeta_master_descuentos/mtarjeta_master_consumo)]
dataset[,total_seguros:= (cseguro_vida+cseguro_auto+cseguro_vivienda+cseguro_accidentes_personales)]
dataset[,total_consumo_sobre_sueldo:= (mautoservicio+mtarjeta_visa_consumo+mtarjeta_master_consumo) / (mpayroll + mpayroll2)]
dataset[,sueldo_sobre_antiguedad:= (mpayroll + mpayroll2) / cliente_antiguedad]
dataset[,payroll_binaria:=ifelse((mpayroll+mpayroll2)>0, 1,0)]
dataset[,payroll_mrentabilidad:=(mpayroll+mpayroll2)/mrentabilidad_annual]
dataset[,vm_consumo_payroll:=(vm_mconsumospesos + vm_mconsumosdolares)/(mpayroll+mpayroll2)]
dataset[,minversiones:=minversion1_pesos+minversion1_dolares+minversion2]
dataset[,mpasivos_inversiones:=mpasivos_margen/(mpayroll + mpayroll + minversiones)]
dataset[,mpasivos_inversiones_pf:=mpasivos_margen/(mpayroll + mpayroll + minversiones + mplazo_fijo_dolares + mplazo_fijo_pesos)]
dataset[,prestamos_binaria:=ifelse((cprestamos_personales+cprestamos_prendarios + cprestamos_hipotecarios)>0, 1,0)]
dataset[,mprestamos:=(mprestamos_personales+mprestamos_prendarios + mprestamos_hipotecarios)]
dataset[,mprestamos_payroll:=mprestamos/(mpayroll + mpayroll2)]
dataset[,mprestamos_antiguedad:=mprestamos/cliente_antiguedad]
dataset[,mprestamos_edad:=mprestamos/cliente_edad]
dataset[,mprestamos_edad2:=mprestamos/(cliente_edad)^2]
dataset[,mprestamos_antiguedad2:=mprestamos/(cliente_antiguedad)^2]
dataset[,sueldo_total:=mpayroll+mpayroll2]
dataset[,rent_anual_sueldo:=mrentabilidad_annual/sueldo_total]
dataset[,minversiones_payroll:=minversiones/sueldo_total]
dataset[,minversiones_mprestamos:=minversiones+mprestamos]
dataset[,minversiones_mprestamos_binaria:=ifelse((minversiones + mprestamos)>0,1,0)]

vnumerador=c("mrentabilidad",
             "mrentabilidad_annual",
             "mcomisiones",
             "mactivos_margen",
             "mpasivos_margen",
             "mcuenta_corriente",
             "mcaja_ahorro",
             "mcaja_ahorro_adicional",
             "mcaja_ahorro_dolares",
             "mcuentas_saldo",
             "mtarjeta_visa_consumo",
             "mtarjeta_master_consumo",
             "mprestamos_personales",
             "mprestamos_prendarios",
             "mprestamos_hipotecarios",
             "mplazo_fijo_dolares",
             "mplazo_fijo_pesos",
             "minversion1_pesos",
             "minversion1_dolares",
             "minversion2",
             "mpayroll",
             "mpayroll2",
             "mcuenta_debitos_automaticos",
             "mtarjeta_visa_debitos_automaticos",
             "mttarjeta_master_debitos_automaticos",
             "mpagodeservicios",
             "mpagomiscuentas",
             "mcajeros_propios_descuentos",
             "mtarjeta_visa_descuentos",
             "mtarjeta_master_descuentos",
             "mcomisiones_mantenimiento",
             "mcomisiones_otras",
             "mforex_buy",
             "mforex_sell",
             "mtransferencias_recibidas",
             "mtransferencias_emitidas",
             "mextraccion_autoservicio",
             "mcheques_depositados",
             "mcheques_emitidos",
             "mcheques_depositados_rechazados",
             "mcheques_emitidos_rechazados",
             "matm",
             "matm_other",
             "Master_msaldototal",
             "Master_msaldopesos",
             "Master_msaldodolares",
             "Master_mconsumospesos",
             "Master_mconsumosdolares",
             "Master_mlimitecompra",
             "Master_madelantopesos",
             "Master_madelantodolares",
             "Master_mpagominimo",
             "Visa_mfinanciacion_limite",
             "Visa_msaldototal",
             "Visa_msaldopesos",
             "Visa_msaldodolares",
             "Visa_mconsumospesos",
             "Visa_mconsumosdolares",
             "Visa_mlimitecompra",
             "Visa_madelantopesos",
             "Visa_madelantodolares",
             "Visa_mpagado",
             "Visa_mpagospesos",
             "Visa_mpagosdolares",
             "Visa_mconsumototal",
             "vm_mfinanciacion_limite",
             "vm_msaldototal",
             "vm_msaldopesos",
             "vm_msaldodolares",
             "vm_mconsumospesos",
             "vm_mconsumosdolares",
             "vm_mlimitecompra",
             "vm_madelantopesos",
             "vm_madelantodolares",
             "vm_mpagado",
             "vm_mpagospesos",
             "vm_mpagosdolares",
             "vm_mconsumototal",
             "vm_mpagominimo",
             "total_consumo",
             "total_seguros",
             "minversiones",
             "mprestamos",
             "sueldo_total",
             "Master_mpagado",
             "Master_mpagospesos",
             "Master_mpagosdolares",
             "Master_mconsumototal")
             
vdenominador=c("cliente_edad",
               "cliente_antiguedad",
               "cproductos",
               "tcuentas",
               "ccuenta_corriente",
               "ctarjeta_debito",
               "ctarjeta_debito_transacciones",
               "ctarjeta_visa",
               "ctarjeta_visa_transacciones",
               "ctarjeta_master",
               "ctarjeta_master_transacciones",
               "cprestamos_personales",
               "cprestamos_prendarios",
               "cprestamos_hipotecarios",
               "cplazo_fijo",
               "cinversion1",
               "cinversion2",
               "cseguro_vida",
               "cseguro_auto",
               "cseguro_vivienda",
               "cseguro_accidentes_personales",
               "cpayroll_trx",
               "cpayroll2_trx",
               "ccuenta_debitos_automaticos",
               "ctarjeta_visa_debitos_automaticos",
               "ctarjeta_master_debitos_automaticos",
               "cpagodeservicios",
               "cpagomiscuentas",
               "ccajeros_propios_descuentos",
               "ctarjeta_visa_descuentos",
               "ctarjeta_master_descuentos",
               "ccomisiones_mantenimiento",
               "ccomisiones_otras",
               "cforex",
               "cforex_buy",
               "cforex_sell",
               "ctransferencias_recibidas",
               "ctransferencias_emitidas",
               "cextraccion_autoservicio",
               "ccheques_depositados",
               "ccheques_emitidos",
               "ccheques_depositados_rechazados",
               "ccheques_emitidos_rechazados",
               "chomebanking_transacciones",
               "ccajas_transacciones",
               "ccajas_consultas",
               "ccajas_depositos",
               "ccajas_extracciones",
               "ccajas_otras",
               "catm_trx",
               "catm_trx_other",
               "ctrx_quarter",
               "Master_cconsumos",
               "Master_cadelantosefectivo",
               "Visa_cconsumos",
               "Visa_cadelantosefectivo",
               "vm_cconsumos",
               "vm_cadelantosefectivo")
               
i=1

for (num in vnumerador)
{
  for (denom in vdenominador) {
    cat(num, denom, " ")
    dataset[, paste0("feintra", i) := 
              (get(num)) / get(denom)]
    i=i+1
  }
  
}