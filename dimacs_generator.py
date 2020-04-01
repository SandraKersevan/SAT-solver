import math

def dimacsformat_sudoku(sez, dim):
    n1 = len(sez)

    root = int(math.sqrt(dim))
    d = dim
    d2 = dim*dim
    d3 = dim*dim*dim

    ##  Potrebni seznami števil
    s = list(range(1, dim + 1))  ## seznam [1, 2, ..., dim]

    i = 0                        ## seznam [[1,..., koren],...,[ ,..., dim]]
    s2 = []
    while i < len(s):
        s2.append(s[i:i+root])
        i += root

    if dim == 4:
      n2 = 448 + n1
    elif dim == 9:
      n2 = 11988 + n1
    else:
      print("Dodaj pogoj!")
      

    text1 = "c The " + str(d) + "*" + str(d) + " Sudoku puzzle \n"
    text2 = "c Atom pxyn is represented by " + str(d2) + "*(x-1) + " + str(d) + "*(y-1) + n \n"
    text3 = "p cnf " + str(d3) + " " + str(n2) + "\n"

    ##  Števila v sudokuju iz vhodnega seznama
    text4 = ""
    end = " 0 \n"
    for trojka in sez:
        x,y,n = trojka
        izracun = d2 * (x-1) + d * (y-1) + n
        text4 = text4 + str(izracun) + end

    ##  V vsakem kvadratku eno število
    text5 = ""
    for x in s:
        for y in s:
            for n in s:
                if n == dim:
                    delniizracun = d2 * (x-1) + d * (y-1) + n
                    text5 = text5 + str(delniizracun) + end
                else:
                     delniizracun = d2 * (x-1) + d * (y-1) + n
                     text5 = text5 + str(delniizracun) + " "

    ##  V vsaki vrstici
    text6 = ""
    for x in s:
        for n in s:
            for y in s:
                if y == dim:
                    delniizracun = d2 * (x-1) + d * (y-1) + n
                    text6 = text6 + str(delniizracun) + end
                else:
                     delniizracun = d2 * (x-1) + d * (y-1) + n
                     text6 = text6 + str(delniizracun) + " "

    ##  V vsakem stolpcu
    text7 = ""
    for y in s:
        for n in s:
            for x in s:
                if x == dim:
                    delniizracun = d2 * (x-1) + d * (y-1) + n
                    text7 = text7 + str(delniizracun) + end
                else:
                     delniizracun = d2 * (x-1) + d * (y-1) + n
                     text7 = text7 + str(delniizracun) + " "

    ##  V vsakem dim*dim kvadratu
    text8 = ""
    for xs in s2:
        for ys in s2:
            for n in s:
                for x in xs:
                    for y in ys:
                        if (x == xs[-1]) & (y == ys[-1]):
                            delniizracun = d2 * (x-1) + d * (y-1) + n
                            text8 = text8 + str(delniizracun) + end
                        else:
                             delniizracun = d2 * (x-1) + d * (y-1) + n
                             text8 = text8 + str(delniizracun) + " "

    ##  Dve različni števili v istem kvadratu
    text9 = ""
    for x in s:
        for y in s:
            for n1 in range(1, dim):
                for n2 in range(n1+1,dim+1):
                        izracun1 = d2 * (x-1) + d * (y-1) + n1
                        izracun2 = d2 * (x-1) + d * (y-1) + n2
                        text9 = text9 + str(-izracun1) + " " + str(-izracun2) + end

    ##  Dve različni - y
    text10 = ""
    for x in s:
        for n in s:
            for y1 in range(1, dim):
                for y2 in range(y1+1,dim+1):
                        izracun1 = d2 * (x-1) + d * (y1-1) + n
                        izracun2 = d2 * (x-1) + d * (y2-1) + n
                        text10 = text10 + str(-izracun1) + " " + str(-izracun2) + end
    
    ##  Dve različni - x
    text11 = ""
    for y in s:
        for n in s:
            for x1 in range(1, dim):
                for x2 in range(x1+1,dim+1):
                        izracun1 = d2 * (x1-1) + d * (y-1) + n
                        izracun2 = d2 * (x2-1) + d * (y-1) + n
                        text10 = text10 + str(-izracun1) + " " + str(-izracun2) + end
    ##  V vsakem dim*dim kvadratu
    text11 = ""
    for xs in s2:
        for ys in s2:
            for n in s:
                for x1 in range(0,len(xs)):
                    for y1 in range(0,len(ys)):
                        for x2 in range(0, len(xs)):
                            for y2 in range(0, len(ys)):
                                if (x1 == x2) & (y1 == y2):
                                    continue
                                elif (x1 > x2):
                                    continue
                                elif (y1 > y2) & (x1 == x2):
                                    continue
                                else:                               
                                    izracun1 = d2 * (xs[x1]-1) + d * (ys[y1]-1) + n
                                    izracun2 = d2 * (xs[x2]-1) + d * (ys[y2]-1) + n
                                    text11 = text11 + str(-izracun1) + " " + str(-izracun2) + end


    

    dat = open(r"dimacs.txt","w")
    dat.write(text1)
    dat.write(text2)
    dat.write(text3)
    dat.write(text4)
    dat.write(text5)
    dat.write(text6)
    dat.write(text7)
    dat.write(text8)
    dat.write(text9)
    dat.write(text10)
    dat.write(text11)

# example of sudoku
dimacsformat_sudoku([(1,1,7),(1,3,2),(1,4,9),(1,5,4),(1,8,5),(2,1,8),
                     (2,2,6),(2,6,3),(3,2,4),(3,3,9),(4,5,7),(4,7,4),
                     (4,9,2),(5,1,6),(5,9,3),(6,1,4),(6,3,3),(6,5,2),
                     (7,7,1),(7,8,6),(8,4,5),(8,8,8),(8,9,7),(9,2,5),
                     (9,5,6),(9,6,1),(9,7,2),(9,9,9)],9)
