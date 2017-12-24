list.of.packages <- c("digest", "httr","jsonlite")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(digest)
require(jsonlite)
require(httr)

Blockchain <- function ()
{
  bc = list (
    chain = list(),
    currentTransactions  = list(),
    nodes = list()
  )
  #' Create a new Block in the Blockchain
  #'
  #' @param proof <int> The proof given by the Proof of Work algorithm
  #' @param previousHash <str> Hash of previous Block
  #' @return new block generated given the \code{proof} and the \code{previousHash}
  #' @examples  
  #' blockchain = Blockchain()
  #' blockchain$nextBlock(previousHash=1, proof=100) # genesis block
  bc$nextBlock = function (proof, previousHash=NULL){
    previousHash <- ifelse (is.null(previousHash), bc$hashBlock(bc$chain[length(bc$chain)]), previousHash)
    block = list('block' = list('index' = length (bc$chain) + 1, 'timestamp' = as.numeric(Sys.time()) , 'transactions' =  bc$currentTransactions, 'proof' = proof, 'previousHash' = previousHash))
    bc$currentTransactions = NULL
    bc$chain <- append(bc$chain, block)
    return (block)
  }
  #' Returns the last block in the Blockchain
  #'
  #' @examples  
  #' blockchain$lastBlock()
  bc$lastBlock = function () {
    bc$chain[length(bc$chain)]
  }
  #' Register a new transaction in the Blockchain
  #'
  #' @param sender <str> address of the sender
  #' @param recipient <str> address of the recipient
  #' @param amount <int> transaction amount
  #' @return  <int> Index of the Block that will hold this transaction
  bc$addTransaction = function (sender, recipient, amount) 
  {
    txn <-  list('transaction'= list('sender'=sender,'recipient'=recipient,'amount'=amount))
    bc$currentTransactions <- append(bc$currentTransactions, txn)
    last.block <- bc$lastBlock()
    return(last.block$block$index + 1)
  }
  #' Hash a block using SHA256
  #'
  #' @param block <block> 
  #' @return  <str> SHA256 hashed value for \code(block)
  #' @examples  
  bc$hashBlock = function (block) {
    require(digest)
    digest(block,algo="sha256")
  }
  
  #' Find a number p' such that hash(pp') contains leading 4 zeroes, where p is the previous p'
  #' p is the previous proof and p' is the new proof
  #' @param last_proof <block> 
  #' @return  <str> SHA256 hashed value for \code(block)
  bc$proofOfWork <- function (last_proof)
  {
    proof <- 0
    while (!bc$validProof(last_proof, proof))
    {
      proof <- proof + 1
    }
    return (proof)
  }

  #' Find a number p' such that hash(pp') ends with two zeroes, where p is the previous p'
  #' p is the previous proof and p' is the new proof
  #' @param last_proof <int> previous proof 
  #' @param proof <int> proof
  #' @return  <bool> TRUE if correct, FALSE if not
  bc$validProof <- function (last_proof, proof) 
  {
    guess = paste0(last_proof,proof)
    guess_hash = digest(guess, algo = 'sha256')
    return (gsub('.*(.{2}$)', '\\1',guess_hash) == "00")
  }
  #' Checks whether a given blockchain is valid
  #'
  #' @return  <bool> TRUE if the chain is valid, FALSE otherwise
  bc$validChain <- function (chain)
  {
    lastBlock <- chain[0]
    currentIndex <- 1
    while (currentIndex < length(chain))
    {
      block = chain[currentIndex]
      # checking for valid linking
      if (block$block$previousHash != bc$hashBlock(lastBlock)) {
        return(FALSE)
      }
      # checking for proof validity
      if(!bc$validProof(lastBlock$block$proof, block$block$proof))
      {
        return (FALSE)
      }
      lastBlock <- block
      currentIndex <- currentIndex +1
    }
    return(TRUE)
  }
  #' Add a new node to the list of existing nodes
  #' 
  #' @param address <str> full URL of the node  
  #' @examples  
  #' blockchain = Blockchain()
  #' blockchain$registerNode('http://192.168.0.5:5000')
  bc$registerNode <- function(address)
  {
    parsed_url = address
    bc$nodes<- append(bc$nodes, parsed_url)
  }
  #' Resolve conflicts by replacing the current chain by the longest chain in the network
  #'
  #' @return  <bool> TRUE if the chain was replaced, FALSE otherwise
  bc$handleConflicts <- function()
  {
    neighbours <- bc$nodes 
    new_chain <- NULL
    max_length = length(bc$chain)
    for (i in 1:length(neighbours))
    {
      chain.node <- GET(paste0(neighbours[i],'/chain'))
      node.chain.length <- jsonlite::fromJSON(chain.node)$length
      node.chain.chain <- jsonlite::fromJSON(chain.node)$chain
      if (node.chain.length > max_length)
      {
        new_chain = node.chain.chain
        max_length<-node.chain.length
      }
    }
    if (!is.null(new_chain))
    {
      bc$chain <- new_chain 
    }
  }
  # Adding bc to the environment
  bc <- list2env(bc)
  class(bc) <- "BlockchainClass"
  return(bc)
}
