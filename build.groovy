@Grab(group='com.ibm.dbb', module='dbb', version='2.0.0')

import com.ibm.dbb.build.*

println("=== Iniciando build COBOL simple con DBB ===")

def srcDir = new File("B45617.LIB.PROGRAM")
def buildDir = new File("build")
buildDir.mkdirs()

srcDir.eachFileMatch(~/.*\.cbl/) { file ->
    println "Compilando: ${file.name}"

    def compile = new CreatePDS(member: 'OBJ', pdsName: 'B45617.OBJ', options: 'REPLACE')
    compile.execute()

    def cbl = new Compile()
    cbl.setLanguage("COBOL")
    cbl.setSource(file)
    cbl.setLogFile(new File(buildDir, "${file.name}.log"))
    cbl.setCblParms("LIB,LIST,MAP")
    cbl.execute()
}

println("=== Build finalizado ===")
