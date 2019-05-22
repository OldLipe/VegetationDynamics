--[[
    @title Um modelo simples de Autômato celular para estudos de dinâmica de 
    vegetação

    @subtitle Disciplina: CAP-241

    @name Felipe Carvalho
    @name Willian Vieira
--]]


--[[
    @name grow Crescimento vegetativo
    @name seed Fecundidade de sementes
    @name tissue Resiliência do tecido
--]]
grow = {0.0, 1.0, 0.2, 0.0, 0.6, 0.6, 0.1, 0.4}
seed = {0.0, 0.2, 0.2, 1.0, 0.2, 0.6, 0.6, 0.5}
tissue = {0.0, 0, 0.95, 0.0, 0.8, 0.0, 0.8, 0.75}

-- Utilizado para garantir que o seed é gerado apenas uma vez
exec = 1 

-- Iterações
iterations = 100

-- Dimensões X e Y
grid_size = 64 

time_ = os.time()

-- 
disturbance = 0.09--0.54--0.81
resource = 0.9

--[[
    @name: vegDeath.function
    @descripition Estado da célula é alterado para zero. 
--]]
vegDeath = function(cell)
    cell.stateNow = 0
    cell.stateNext = 0
end -- vegDeath.function

--[[
    @name: Maintain.function
    @descripition: Verifica se existem recursos suficientes para manter a planta
    ou se a resiliência do tecido da planta presente na célula é suficiente para
    que ela sobreviva.
--]]
Maintain = function(cell)
    if math.random() < resource or math.random() < tissue[cell.stateNow+1] then
        cell.stateNext = cell.stateNow
    else
        vegDeath(cell)
    end -- if
end -- Maintain.function


--[[
    @name: Grow.function
    @descripition Determina-se a probabilidade de um tipo de planta ser
    substituído por outro.  
--]]
Grow = function(cell)
    local gw = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
    local sw = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}
    local well = {0, 0, 0, 0, 0, 0, 0, 0, 0}
    local choice = 0.0
    local gtotal = 0.0
    local stotal = 0.0
    local i = 1 -- O índice de vetores, em Lua, inicia em 1

    forEachNeighbor(cell, function(neigh)
        gtotal = gtotal + grow[neigh.stateNow + 1]
        stotal = stotal + seed[neigh.stateNow + 1]
        gw[i] = gtotal
        sw[i] = stotal
        well[i] = neigh.stateNow
        i = i + 1
        if i == 5 then -- Caso seja o pixel central.
                        -- A função forEachNeighbor pula esta célula
            gtotal = gtotal + grow[cell.stateNow + 1]
            stotal = stotal + seed[cell.stateNow + 1]
            gw[i] = gtotal
            sw[i] = stotal
            well[i] = cell.stateNow
            i = i + 1
        end -- índice 5 refere-se ao pixel central
    end) -- forEachNeighbor


    i = 1
    choice = math.random() * 9.0

    --[[
    A cell is first tested for the possibility of occupation
    by vegetative growth
    --]]
    if choice < gtotal then
        while (gw[i] < choice) do
            i = i + 1
        end
        cell.stateNext = well[i]
    end -- if

    --[[
    Only if this is unsuccessful is it tested for growth from seed.
    --]]
    if (cell.stateNext == 0) then
        choice = math.random() * stotal
        if (math.random() < (1 - math.exp(-stotal/3.0))) then
            i = 1
            while (sw[i] < choice) do
                i = i + 1
            end
            cell.stateNext = well[i]
        end -- if
    end -- if
end -- Grow

--[[
    @name:Part1.function
    @descripition Executa as funções VegDeath e Maintain   
--]]
Part1 = function(cell)
    if math.random() < disturbance then
        vegDeath(cell)
    else
        Maintain(cell)
    end -- if
end -- Part1.function

--[[
    @name:Part2.function
    @descripition Executa a função Grow caso existam recursos suficientes   
--]]
Part2 = function(cell)
        if(math.random() < resource) then
            Grow(cell)
        end -- if
end -- Part2


function os.sleep(msec)
    local now = os.clock() + msec/1000
    repeat until os.clock() >= now
end -- os.sleep

-- ############## Vegetation Dynamics - Model ##################################
Veg_Dynamics = Model{
    print("Model!"),
    -- Time and Dimension
    finalTime = iterations, -- running time (iterations)
    save = 1, -- Would you like to save the maps? [Yes: 1]
    iter = 1,
    dir = "C:/Users/Willian/Desktop/mapteste/",--C1_2/",
    dim = grid_size, -- X and Y Dimensions

    execute = function(model)
        forEachCell(model.cs, function(cell)
            Part2(cell)

            n = cell.stateNow
            cell.stateNow = cell.stateNext
            cell.stateNext = n
        end) -- forEachCell

        if (model.save == 1) then
            filename = model.dir .. "Map" .. model.iter .. ".png"
            print(filename)
            model.map:save(filename)
        end
        model.iter = model.iter + 1

        os.sleep(1000) -- Delay between iterations (1000 Milliseconds = 1 Second)
    end, -- execute

    init = function(self)

        self.cell = Cell
        {
            print("Cell!"),
            stateNow = 0, -- Initiate all cell with this value
            stateNext = 0,

            init = function()--cell
            end,

            execute = function(model)
                if exec == 1 then
                    math.randomseed( time_ )
                    exec = exec + 1
                end
                Part1(model)
                n = model.stateNow
                model.stateNow = model.stateNext
                model.stateNext = n
            end -- execute
        }

        self.cs = CellularSpace
        {
            xdim = self.dim,
            instance = self.cell,
        }
        self.cs:createNeighborhood{strategy="moore"}

        tipo = 7
        while(tipo>=0) do
            i = 200
            while(i>0) do
                -- atribui um estado aleatório para um pixel aleatório
                self.cs:sample().stateNow = tipo
                i = i - 1
            end -- while
            tipo = tipo - 1
        end -- while

        self.map = Map
        {
            target = self.cs,
            select = "stateNow",
            min = 0,
            max = 7,
            slices = 8,
            grouping = "uniquevalue",
            value = {0, 1, 2, 3, 4, 5, 6, 7},
            color= {{216,179,101}, {0,68,27}, {0,109,44}, {35,139,69}, 
            {65,174,118}, {102,194,164}, {153,216,201}, {204,236,230}}
        }

        self.timer = Timer
        {
            Event{action = self},
            Event{action = self.cs},
            Event{action = self.map}
        }
    end -- init

} -- Veg_Dynamics = Model{

AC = Veg_Dynamics{}
AC:run()
