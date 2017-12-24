list.of.packages <- c("uuid")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(uuid)
# make sure you put the path of your blockchain.R file
source('blockchain.R')

# Generate a globally unique address for this node
node_identifier = gsub('-','',UUIDgenerate())
# Instantiate the Blockchain
blockchain = Blockchain()
# genesis block
blockchain$nextBlock(previousHash=1, proof=100)
#* @serializer custom_json
#* @post /transactions/new
function(req)
{
  #eg req_json <- '{"sender": "my address", "recipient": "someone else address", "amount": 5}'
  #values <- jsonlite::fromJSON(req_json)
  values <- jsonlite::fromJSON(req$postBody)

  # Check that the required fields are in the POST'ed data
  required = c('sender','recipient', 'amount')
  if (!all(required %in% names(values))) {
    return ('Missing Values - sender, recipient and amount are required')
  }
  index = blockchain$addTransaction(values$sender, values$recipient, values$amount)
  
  list('message' = paste('Transaction will be added to Block', index))
}

#* @serializer custom_json
#* @get /chain
function(req)
{
  list('chain'=blockchain$chain, 'length'=length(blockchain$chain))
}
#* @serializer custom_json
#* @get /mine
function(req)
{
  # We run the proof of work algorithm to get the next proof
  lastBlock = blockchain$lastBlock()
  lastProof = lastBlock$block$proof
  proof = blockchain$proofOfWork(lastProof)
  
  # We must receive a reward for finding the proof.
  # The sender is "0" to signify that this node has mined a new coin.
  blockchain$addTransaction(sender="0",recipient = node_identifier, amount=1)

  # Forge the new block by adding it to the chain
  previousHash = blockchain$hashBlock(lastBlock)
  block = blockchain$nextBlock(proof, previousHash)
  list('message'='New block forged', 'index'= block$block$index, 'transactions'= block$block$transactions, 'proof'=block$block$proof,'previousHash'=block$block$previousHash)
#  list('message'='New block forged', c('index'= block$block$index, 'transactions'= block$block$transactions, 'proof'=block$block$proof,'previousHash'=block$block$previousHash))
}
#* @serializer custom_json
#* @post /nodes/register
function (req)
{
#  req_json <- '{"sender": "my address", "recipient": "someone else address", "amount": 5}'
  values <- jsonlite::fromJSON(req$postBody)
  nodes <-  values$nodes
  if (is.null(nodes))
  {
    return("Error: the list of nodes is not valid")
  }
  for (i in 1:length(nodes))
  {
    blockchain$registerNode(nodes[i])
  }
  TRUE
}
#* @serializer custom_json
#* @get /nodes/resolve
function (req)
{
  replaced = blockchain$handleConflicts()
  if (replaced)
  {
    list('message'='Replaced', 'chain'=blockchain$chain)
  } else  {
    list('message'='Authoritative block chain - not replaceable ', 'chain'=blockchain$chain)
  }
}
#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-", 
      req$REQUEST_METHOD, req$PATH_INFO, "-", 
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}
#* @serializer custom_json
#* @get /chain/show
#* @html
function(req)
{
  render.html <- ""
  
  paste0(render.html, '<br>')
  render.html <- paste0(render.html, 'Current transactions:<br>')
  for (i in 1:length(blockchain$currentTransactions))
  {
    render.html <- paste0(render.html, 'Transaction' , i ,'<br>')
    render.html <- paste0(render.html, 'sender:', blockchain$currentTransactions[i]$transaction$sender)
    render.html <- paste0(render.html, '<br>')
    render.html <- paste0(render.html, 'recipient:', blockchain$currentTransactions[i]$transaction$recipient)
    render.html <- paste0(render.html, '<br>')
    render.html <- paste0(render.html, 'amount:', blockchain$currentTransactions[i]$transaction$amount)
    render.html <- paste0(render.html, '<br>')
  }
  render.html <- paste0(render.html, '<br>')
  render.html <- paste0(render.html, 'Current transactions:<br>')
  render.html <- paste0(render.html, '<div>','<br>')
  for (i in 1:blockchain$lastBlock()$block$index)
  {
    render.html <- paste0(render.html, '<br>')
    render.html <- paste0(render.html, '<b>Block nr:</b>', blockchain$chain[i]$block$index)
    render.html <- paste0(render.html, '<br>')
    render.html <- paste0(render.html, '<b>Transactions</b>')
    render.html <- paste0(render.html, '<br>')
    render.html <- paste0(render.html, blockchain$chain[i]$block$transactions)
    render.html <- paste0(render.html, '<br>')
    render.html <- paste0(render.html, '<b>Proof</b>')
    render.html <- paste0(render.html, '<br>')
    render.html <- paste0(render.html,blockchain$chain[i]$block$proof)
    render.html <- paste0(render.html, '<br>')
  }
  render.html <- paste0(render.html, '</div>')
  render.html
}

