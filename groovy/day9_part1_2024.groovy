
def solve() {
    def input = new File('input.txt').text.trim()

    def lengths = []
    for (int i = 0; i < input.length(); i++) {
        lengths << Integer.parseInt(input[i])
    }

    def blocks = []
    def fileIds = []
    int fileIdCounter = 0
    boolean isFile = true

    for (int length : lengths) {
        if (isFile) {
            blocks.addAll((0..<length).collect { fileIdCounter })
            fileIds << fileIdCounter
            fileIdCounter++
        } else {
            blocks.addAll((0..<length).collect { null })
        }
        isFile = !isFile
    }

    // Compact the disk
    int freeIndex = 0
    int lastIndex = blocks.size() - 1

    while (freeIndex < blocks.size() && blocks[freeIndex] != null) {
        freeIndex++
    }
    
    while (lastIndex >= 0 && blocks[lastIndex] == null){
        lastIndex--
    }


    while (freeIndex < lastIndex) {
       if(blocks[freeIndex] == null){
            blocks[freeIndex] = blocks[lastIndex]
            blocks[lastIndex] = null
            
            while (freeIndex < blocks.size() && blocks[freeIndex] != null) {
                freeIndex++
            }
           
           while (lastIndex >= 0 && blocks[lastIndex] == null){
                lastIndex--
            }
        
       } else {
           freeIndex++
       }
    }

    // Calculate checksum
    long checksum = 0
    for (int i = 0; i < blocks.size(); i++) {
        if (blocks[i] != null) {
            checksum += (long) i * blocks[i]
        }
    }

    println checksum
}

solve()
