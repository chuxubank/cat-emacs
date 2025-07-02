// preview.init.gradle.kts

import org.gradle.api.initialization.Settings
import org.gradle.api.Project
import org.gradle.kotlin.dsl.*

// This block is executed when the settings.gradle(.kts) file has been evaluated.
// We use it to dynamically add our preview-host module to the build.
settingsEvaluated {
    include(":preview-host")
}

// This block is executed for the root project of the build.
rootProject {
    // We use afterEvaluate to ensure all subprojects have been loaded and configured.
    afterEvaluate {
        // Find all subprojects that have the Jetpack Compose plugin applied.
        val composeProjects = subprojects.filter { it.plugins.hasPlugin("org.jetbrains.compose") }

        if (composeProjects.isEmpty()) {
            logger.warn("Warning: No modules with 'org.jetbrains.compose' plugin found. The preview might be empty.")
        }

        // Get our dynamically added :preview-host project and configure it.
        project(":preview-host") {
            apply(plugin = "org.jetbrains.kotlin.jvm")
            apply(plugin = "org.jetbrains.compose")

            repositories {
                google()
                mavenCentral()
                maven("https://maven.pkg.jetbrains.space/public/p/compose/dev")
            }

            dependencies {
                // Use the Compose BOM to manage dependency versions
                implementation(platform("org.jetbrains.compose:compose-bom:1.6.1"))
                implementation("org.jetbrains.compose.desktop:desktop")

                // Add all the user's Compose modules as dependencies to our preview host.
                composeProjects.forEach { composeProject ->
                    implementation(composeProject)
                }
            }

            // Configure the Compose for Desktop application settings.
            extensions.getByType<org.jetbrains.compose.desktop.application.dsl.Application>().apply {
                mainClass = "com.example.preview.MainKt"
            }

            val generatedSrcDir = buildDir.resolve("generated/preview/src/main/kotlin")
            sourceSets.main.get().kotlin.srcDir(generatedSrcDir)

            // This task generates the Main.kt file for our preview application.
            val generateTask = tasks.register("generatePreviewSource") { 
                doLast {
                    val previewFunctions = mutableListOf<String>()
                    composeProjects.forEach { proj ->
                        proj.sourceSets["main"].kotlin.srcDirs.forEach { dir ->
                            dir.walkTopDown().forEach { file ->
                                if (file.isFile && file.extension == "kt") {
                                    val content = file.readText()
                                    if (content.contains("@Preview")) {
                                        val packageName = content.lines().firstOrNull { it.startsWith("package ") }?.removePrefix("package ")?.trim()
                                        // Correctly escaped regex to find preview functions
                                        val regex = Regex("@Preview\s*@Composable\s*fun\s+([\w\d_]+)\s*\(")
                                        regex.findAll(content).forEach { match ->
                                            val funName = match.groupValues[1]
                                            if (packageName != null && funName.isNotBlank()) {
                                                previewFunctions.add("$packageName.$funName")
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // Generate the content of the Main.kt file.
                    val mainKtContent = """
                        package com.example.preview

                        import androidx.compose.foundation.layout.*
                        import androidx.compose.foundation.rememberScrollState
                        import androidx.compose.foundation.verticalScroll
                        import androidx.compose.material.Text
                        import androidx.compose.material.Divider
                        import androidx.compose.runtime.Composable
                        import androidx.compose.ui.Modifier
                        import androidx.compose.ui.graphics.Color
                        import androidx.compose.ui.unit.dp
                        import androidx.compose.ui.window.Window
                        import androidx.compose.ui.window.application
                        import androidx.compose.ui.window.rememberWindowState
                        ${previewFunctions.distinct().joinToString("\n") { "import $it" }}

                        fun main() = application {
                            Window(onCloseRequest = ::exitApplication, state = rememberWindowState(width = 800.dp, height = 1200.dp), title = "Compose Preview Gallery") {
                                Column(modifier = Modifier.fillMaxSize().verticalScroll(rememberScrollState())) {
                                    ${if (previewFunctions.isEmpty()) """
                                        Text("No @Preview functions found.")
                                    """ else previewFunctions.distinct().joinToString("\n") { fqn ->
                                        val funName = fqn.substringAfterLast('.')
                                        """
                                        Column(modifier = Modifier.padding(16.dp)) {
                                            Text("\"$fqn\"", color = Color.Gray)
                                            Spacer(modifier = Modifier.height(8.dp))
                                            Box(modifier = Modifier.fillMaxWidth()) { $funName() }
                                        }
                                        Divider()
                                        """
                                    }}
                                }
                            }
                        }
                    """.trimIndent()

                    val outputFile = generatedSrcDir.resolve("com/example/preview/Main.kt")
                    outputFile.parentFile.mkdirs()
                    outputFile.writeText(mainKtContent)
                }
            }

            // Make sure our source generation task runs before compilation.
            tasks.named("compileKotlin").configure { dependsOn(generateTask) }
        }
    }

    // Register the top-level task that the user will run.
    tasks.register("runPreview") {
        group = "Preview"
        description = "Runs the Jetpack Compose preview gallery."
        dependsOn(":preview-host:run")
    }
}
