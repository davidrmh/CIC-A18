

def createC1(dataset):
    '''
    Creates candidate itemset of size one
    dataset is a list of list where each list is
    a transaction and the element inside of that list
    is the article id.
    '''
    c1=[]
    for transaction in dataset:
        for item in transaction:
            if not [item] in c1:
                c1.append([item])
    c1.sort()
    return map(frozenset,c1)

def scanD(dataset,candidates,minsupport):
    '''
    Calculates support for every itemset in candidates
    dataset is created with map(set,dataset)
    where dataset is a list of list.
    '''
    sscnt={}
    for tid in dataset:
        for can in candidates:
            if can.issubset(tid): #If itemset is present in transaction
                if not sscnt.has_key(can): sscnt[can]=1
                else: sscnt[can] +=1
    numitems=float(len(dataset))
    retlist=[]
    supportdata={}
    for key in sscnt:
        support = sscnt[key]/numitems
        if support>=minsupport:
            retlist.insert(0,key) #inserts from the left
        supportdata[key]=support
    return retlist,supportdata
