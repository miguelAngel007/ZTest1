import com.ibm.dbb.build.*

def props = BuildProperties.getInstance()
props.load(new File("BuildProperties.properties"))

def program = args.size() > 0 ? args[0] : null
if (!program) {
    println("Uso: groovyz Build.groovy <programa.cbl>")
    System.exit(1)
}

def member = program.replace(".cbl", "").toUpperCase()
def srcFile = new File("${props.sourceDir}/${program}")

println("=== Compilando ${member} ===")

// Copiar fuente al PDS
new CopyToPDS().file(srcFile).dataset(props.srcPDS).member(member).execute()

// Rutas de salida de log
def sysoutLog = new File("${props.workspace}/builds/${member}_cbl.log")
if (!sysoutLog.exists()) sysoutLog.createNewFile()

// Compilación COBOL
def compile = new MVSExec().pgm(props.cobolCompiler).parm("LIB")

compile.dd(new DDStatement().name("SYSIN").dsn("${props.srcPDS}(${member})").options("shr"))
compile.dd(new DDStatement().name("SYSLIN").dsn("${props.objPDS}(${member})").options("new"))

// SYSUT1..SYSUT17
(1..17).each { num ->
    compile.dd(new DDStatement().name("SYSUT$num").options("cyl space(5,5) unit(vio) new"))
}

// SYSMDECK y SYSPRINT
compile.dd(new DDStatement().name("SYSMDECK").options("cyl space(5,5) unit(vio) new"))
compile.dd(new DDStatement().name("SYSPRINT").options("cyl space(5,5) unit(vio) new"))
compile.copy(new CopyToHFS().ddName("SYSPRINT").file(sysoutLog))

// Librería del compilador
compile.dd(new DDStatement().name("TASKLIB").dsn(props.cobolCompilerDS).options("shr"))

def rc = compile.execute()
if (rc > 4) {
    println("Error de compilacion RC=$rc. Revisa el log: ${sysoutLog}")
    System.exit(rc)
} else {
    println("Compilación exitosa RC=$rc. Log: ${sysoutLog}")
}

// Link-edit
def linkLog = new File("${props.workspace}/builds/${member}_link.log")
if (!linkLog.exists()) linkLog.createNewFile()

def link = new MVSExec().pgm(props.cobolLinkEditor)
link.dd(new DDStatement().name("SYSLMOD").dsn("${props.loadPDS}(${member})").options("shr"))
link.dd(new DDStatement().name("SYSLIN").dsn("${props.objPDS}(${member})").options("shr"))
link.dd(new DDStatement().name("SYSPRINT").options("cyl space(5,5) unit(vio) new"))
link.copy(new CopyToHFS().ddName("SYSPRINT").file(linkLog))
link.parm(props.cobolLinkEditParms)

def rcLink = link.execute()
if (rcLink > 4) {
    println("Error en linkedit RC=$rcLink. Revisa el log: ${linkLog}")
    System.exit(rcLink)
} else {
    println("Link-edit completado RC=$rcLink. Log: ${linkLog}")
}
