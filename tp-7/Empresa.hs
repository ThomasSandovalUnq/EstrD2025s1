module Empresa
    (Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, agregarEmpleado
        agregarASector, borrarEmpleado)
    where

import Map

type SectorId = Int

type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)

{- INV REP: Para ConsE mapSec mapCUIL...
            * Si existe un empleado en el Set Empleado de mapSec, este debe de estar representado en el mapCuil con su CUIL como clave.
-}

consEmpresa :: Empresa -- O(1)
--Propósito: construye una empresa vacía.

buscarPorCUIL :: CUIL -> Empresa -> Empleado --O(log E)
--Propósito: devuelve el empleado con dicho CUIL.
--Precondición: el CUIL es de un empleado de la empresa.

empleadosDelSector :: SectorId -> Empresa -> [Empleado] -- O(log S+E)
--Propósito: indica los empleados que trabajan en un sector dado.

todosLosCUIL :: Empresa -> [CUIL] --O(E)
--Propósito: indica todos los CUIL de empleados de la empresa.

todosLosSectores :: Empresa -> [SectorId] --O(S)
--Propósito: indica todos los sectores de la empresa

agregarSector :: SectorId -> Empresa -> Empresa --O(log S)
--Propósito: agrega un sector a la empresa, inicialmente sin empleados

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa --O(S * (logS + logE))
--Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el CUIL dado

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa --O(log E + logS)
--Propósito: agrega un sector al empleado con dicho CUIL.

borrarEmpleado :: CUIL -> Empresa -> Empresa --O(S * (log S + log E))
--Propósito: elimina al empleado que posee dicho CUIL.

consEmpresa                                     = ConsE emptyM emptyM

buscarPorCUIL cuil (ConsE(mapSec mapCUIL))      = buscarCUIL cuil mapCUIL

empleadosDelSector sec (ConsE (mapSec mapCUIL)) = case lookupM sec mapSec of
                                                    Just empleados -> setToList(empleados)
                                                    Nothing        -> error "NO HAY EMPLEADOS EN ESTE SECTOR."

--El costo es O(log S + E) porque el lookupM tiene costo O(log S) y se le suma el E de setToList de los empleados.

todosLosCUIL (ConsE mapSec mapCUIL)                  = keys mapCUIL --keys tiene costo O(E).

todosLosSectores (ConsE mapSec mapCUIL)              = keys mapSec  --keys tiene costo O(S).

agregarSector sec (ConsE mapSec mapCUIL)             = (ConsE (assocM sec (emptyS) mapSec) mapCUIL)

agregarEmpleado sectores cuil (ConsE mapSec mapCUIL) = let empl = nuevoEmpleadoConSec sectores cuil in              -- O(S * (logS + logE)).
                                                        ConsE (actualizarSectoresConNuevoEmp mapSec sectores empl)
                                                                (actualizarCUILsConNuevoEmp mapCUIL cuil empl)

agregarASector sec cuil (ConsE mapSec mapCUIL)       = case lookupM cuil mapCUIL of                                     --O(log E + logS)
                                                        Just empl -> (ConsE (actualizarSectorConEmp mapSec sec empl) mapCUIL)
                                                        Nothing   -> (ConsE mapSec mapCUIL)

borrarEmpleado cuil (ConsE mapSec mapCUIL)           = case lookupM cuil mapCUIL of              --O(log E)  Costo total = O(S * (log S + log E))
                                                        Nothing   -> (ConsE mapSec mapCUIL)
                                                        Just empl -> (ConsE (eliminarEmpleadoDeSectores (sectores empl) empl mapSec) --O(S * (log S))
                                                                                                (deleteM cuil mapCUIL)) --O(log E)

buscarCUIL :: CUIL -> Map CUIL Empleado -> Empleado     
--PRECOND: El cuil dado es de un empleado de la empresa.
buscarCUIL cuil mapEmpl = case lookupM cuil mapEmpl of
                            Just empleado -> empleado
                            Nothing       -> error "EL CUIL DADO DEBE DE EXISTIR EN LA EMPRESA."

--El costo de buscarCUIL es O(log E), ya que el lookupM tiene costo O(log K), donde K es la cantidad de elementos clave y aqui serian empleados.

nuevoEmpleadoConSec :: [SectorId] -> CUIL -> Empleado
nuevoEmpleadoConSec [] cuil      = consEmpleado cuil
nuevoEmpleadoConSec (s:ss) cuils = incorporarSector s (nuevoEmpleadoConSec ss cuil)

actualizarSectoresConNuevoEmp :: Map SectorId (Set Empleado) -> [SectorId] -> Empleado -> Map SectorId (Set Empleado)
actualizarSectoresConNuevoEmp mapSec [] empl     = mapSec
actualizarSectoresConNuevoEmp mapSec (s:ss) empl = case lookupM s mapSec of
                                                    Nothing    -> assocM s (addS empl emptyS) (actualizarSectoresConNuevoEmp mapSec ss empl) 
                                                    Just empls -> assocM s (addS empl empls) (actualizarSectoresConNuevoEmp mapSec ss empl) 

actualizarCUILsConNuevoEmp :: Map CUIL Empleado -> CUIL -> Empleado -> Map CUIL Empleado
actualizarCUILsConNuevoEmp mapCUIL cuil empl = case lookupM cuil mapCUIL of
                                                Nothing -> assocM cuil empl mapCUIL
                                                Just _  -> mapCUIL

actualizarSectorConEmp :: Map SectorId (Set Empleado) -> SectorId -> Empleado -> Map SectorId (Set Empleado) --O(log S)
actualizarSectorConEmp mapS sec empl = case lookupM sec mapSec of                                         --lookupM O(log S)
                                            Nothing    -> assocM sec (addS empl emptyS) mapS
                                            Just empls -> assocM sec (addS empl empls) mapS               --assocM O(log S)

eliminarEmpleadoDeSectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
eliminarEmpleadoDeSectores [] empl mapS     = mapS
eliminarEmpleadoDeSectores (x:xs) empl mapS = case lookupM x mapS of                                            --O(log S)
                                                Nothing    -> eliminarEmpleadoDeSectores xs empl mapS
                                                Just empls -> assocM x (removeS empl empls) (eliminarEmpleadoDeSectores xs empl mapS)   --O(log S)