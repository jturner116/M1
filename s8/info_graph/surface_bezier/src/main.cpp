#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <iostream>
#include <vector>
#include <cmath>

void framebuffer_size_callback(GLFWwindow *window, int width, int height);
void processInput(GLFWwindow *window);
void mouse_callback(GLFWwindow *window, double xpos, double ypos);
void scroll_callback(GLFWwindow *window, double xoffset, double yoffset);

const char *vertexShaderSource = "#version 330 core\n"
                                 "layout (location = 0) in vec3 aPos;\n"
                                 "layout (location = 1) in vec3 aNormal;\n"
                                 "layout (location = 2) in vec2 aTexCoord;\n"
                                 "out vec3 FragPos;\n"
                                 "out vec3 Normal;\n"
                                 "out vec2 TexCoord;\n"
                                 "uniform mat4 model;\n"
                                 "uniform mat4 view;\n"
                                 "uniform mat4 projection;\n"
                                 "void main()\n"
                                 "{\n"
                                 "   FragPos = vec3(model * vec4(aPos, 1.0));\n"
                                 "   Normal = mat3(transpose(inverse(model))) * aNormal;\n"
                                 "   TexCoord = aTexCoord;\n"
                                 "   gl_Position = projection * view * vec4(FragPos, 1.0);\n"
                                 "}\0";

const char *fragmentShaderSource = "#version 330 core\n"
                                   "out vec4 FragColor;\n"
                                   "in vec3 FragPos;\n"
                                   "in vec3 Normal;\n"
                                   "in vec2 TexCoord;\n"
                                   "uniform vec3 lightPos;\n"
                                   "uniform vec3 lightColor;\n"
                                   "uniform vec3 objectColor;\n"
                                   "uniform int renderMode;\n"
                                   "void main()\n"
                                   "{\n"
                                   "    if (renderMode == 0) {\n"
                                   "        // Diffuse shading\n"
                                   "        vec3 norm = normalize(Normal);\n"
                                   "        vec3 lightDir = normalize(lightPos - FragPos);\n"
                                   "        float diff = max(dot(norm, lightDir), 0.0);\n"
                                   "        vec3 diffuse = diff * lightColor;\n"
                                   "        FragColor = vec4(diffuse * objectColor, 1.0);\n"
                                   "    } else if (renderMode == 1) {\n"
                                   "        // Visualize normal\n"
                                   "        FragColor = vec4(normalize(Normal), 1.0);\n"
                                   "    } else {\n"
                                   "        // Visualize parametric space\n"
                                   "        FragColor = vec4(TexCoord, 0.0, 1.0);\n"
                                   "    }\n"
                                   "}\n\0";
glm::vec3 bezierSurface(const std::vector<std::vector<glm::vec3>> &controlPoints, float u, float v)
{
    int m = controlPoints.size() - 1;
    int n = controlPoints[0].size() - 1;
    glm::vec3 point(0.0f);

    for (int i = 0; i <= m; i++)
    {
        for (int j = 0; j <= n; j++)
        {
            float bernsteinI = std::pow(1.0f - u, m - i) * std::pow(u, i) * std::tgamma(m + 1) / (std::tgamma(i + 1) * std::tgamma(m - i + 1));
            float bernsteinJ = std::pow(1.0f - v, n - j) * std::pow(v, j) * std::tgamma(n + 1) / (std::tgamma(j + 1) * std::tgamma(n - j + 1));
            point += bernsteinI * bernsteinJ * controlPoints[i][j];
        }
    }

    return point;
}

glm::vec3 bezierSurfaceNormal(const std::vector<std::vector<glm::vec3>> &controlPoints, float u, float v)
{
    const float epsilon = 0.0001f;
    glm::vec3 pointU = bezierSurface(controlPoints, u + epsilon, v);
    glm::vec3 pointV = bezierSurface(controlPoints, u, v + epsilon);
    glm::vec3 pointCurrent = bezierSurface(controlPoints, u, v);
    glm::vec3 tangentU = pointU - pointCurrent;
    glm::vec3 tangentV = pointV - pointCurrent;
    return glm::normalize(glm::cross(tangentU, tangentV));
}

// Camera parameters
glm::vec3 cameraPos = glm::vec3(0.0f, 0.0f, 3.0f);
glm::vec3 cameraFront = glm::vec3(0.0f, 0.0f, -1.0f);
glm::vec3 cameraUp = glm::vec3(0.0f, 1.0f, 0.0f);
float yaw = -90.0f;
float pitch = 0.0f;
float fov = 45.0f;
float lastX = 800.0f / 2.0;
float lastY = 600.0f / 2.0;
bool firstMouse = true;
int main()
{
    // GLFW: initialize and configure
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

#ifdef __APPLE__
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
#endif

    // GLFW window creation
    GLFWwindow *window = glfwCreateWindow(800, 600, "Curve Display", NULL, NULL);
    if (!window)
    {
        std::cout << "Failed to create GLFW window" << std::endl;
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

    // GLAD: load all OpenGL function pointers
    if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    {
        std::cout << "Failed to initialize GLAD" << std::endl;
        return -1;
    }
    glfwSetCursorPosCallback(window, mouse_callback);
    glfwSetScrollCallback(window, scroll_callback);
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

    // Build and compile our shader program
    // Vertex shader
    int vertexShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertexShader, 1, &vertexShaderSource, NULL);
    glCompileShader(vertexShader);
    // Check for shader compile errors
    int success;
    char infoLog[512];
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);
        std::cout << "ERROR::SHADER::VERTEX::COMPILATION_FAILED\n"
                  << infoLog << std::endl;
    }
    // Fragment shader
    int fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader, 1, &fragmentShaderSource, NULL);
    glCompileShader(fragmentShader);
    // Check for shader compile errors
    glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        glGetShaderInfoLog(fragmentShader, 512, NULL, infoLog);
        std::cout << "ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n"
                  << infoLog << std::endl;
    }
    // Link shaders
    int shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);
    // Check for linking errors
    glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
    if (!success)
    {
        glGetProgramInfoLog(shaderProgram, 512, NULL, infoLog);
        std::cout << "ERROR::SHADER::PROGRAM::LINKING_FAILED\n"
                  << infoLog << std::endl;
    }
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    std::vector<std::vector<glm::vec3>> controlPoints = {
        {glm::vec3(-1.5f, -1.5f, 0.0f), glm::vec3(-0.5f, -1.5f, 0.0f), glm::vec3(0.5f, -1.5f, 0.0f), glm::vec3(1.5f, -1.5f, 0.0f)},
        {glm::vec3(-1.5f, -0.5f, 0.0f), glm::vec3(-0.5f, -0.5f, 1.0f), glm::vec3(0.5f, -0.5f, 1.0f), glm::vec3(1.5f, -0.5f, 0.0f)},
        {glm::vec3(-1.5f, 0.5f, 0.0f), glm::vec3(-0.5f, 0.5f, 1.0f), glm::vec3(0.5f, 0.5f, 1.0f), glm::vec3(1.5f, 0.5f, 0.0f)},
        {glm::vec3(-1.5f, 1.5f, 0.0f), glm::vec3(-0.5f, 1.5f, 0.0f), glm::vec3(0.5f, 1.5f, 0.0f), glm::vec3(1.5f, 1.5f, 0.0f)}};

    // Generate surface points
    int resolutionU = 50;
    int resolutionV = 50;
    std::vector<GLfloat> surfaceVertices;
    std::vector<GLfloat> surfaceNormals;
    std::vector<GLfloat> surfaceTexCoords;

    std::vector<GLfloat> surfaceData;

    for (int i = 0; i <= resolutionU; i++)
    {
        float u = (float)i / resolutionU;
        for (int j = 0; j <= resolutionV; j++)
        {
            float v = (float)j / resolutionV;
            glm::vec3 point = bezierSurface(controlPoints, u, v);
            glm::vec3 normal = bezierSurfaceNormal(controlPoints, u, v);
            surfaceData.push_back(point.x);
            surfaceData.push_back(point.y);
            surfaceData.push_back(point.z);
            surfaceData.push_back(normal.x);
            surfaceData.push_back(normal.y);
            surfaceData.push_back(normal.z);
            surfaceData.push_back(u);
            surfaceData.push_back(v);
        }
    }

    std::vector<unsigned int> indices;
    for (int i = 0; i < resolutionU; i++)
    {
        for (int j = 0; j <= resolutionV; j++)
        {
            indices.push_back(i * (resolutionV + 1) + j);
            indices.push_back((i + 1) * (resolutionV + 1) + j);
        }
    }
    unsigned int VBO, VAO, EBO;
    glGenVertexArrays(1, &VAO);
    glGenBuffers(1, &VBO);
    glGenBuffers(1, &EBO);
    glBindVertexArray(VAO);

    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, surfaceData.size() * sizeof(GLfloat), surfaceData.data(), GL_STATIC_DRAW);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices.size() * sizeof(unsigned int), indices.data(), GL_STATIC_DRAW);

    // Position attribute
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), (void *)0);
    glEnableVertexAttribArray(0);

    // Normal attribute
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), (void *)(3 * sizeof(GLfloat)));
    glEnableVertexAttribArray(1);

    // Texture coordinate attribute
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * sizeof(GLfloat), (void *)(6 * sizeof(GLfloat)));
    glEnableVertexAttribArray(2);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    // Set up lighting
    glm::vec3 lightPos(1.2f, 1.0f, 2.0f);
    glm::vec3 lightColor(1.0f, 1.0f, 1.0f);
    glm::vec3 objectColor(1.0f, 0.5f, 0.31f);
    int renderMode = 1;

    // Render loop
    while (!glfwWindowShouldClose(window))
    {
        processInput(window);

        // Render
        glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // Set up transformations
        glm::mat4 model = glm::mat4(1.0f);
        glm::mat4 view = glm::lookAt(cameraPos, cameraPos + cameraFront, cameraUp);
        glm::mat4 projection = glm::perspective(glm::radians(fov), 800.0f / 600.0f, 0.1f, 100.0f);
        unsigned int modelLoc = glGetUniformLocation(shaderProgram, "model");
        unsigned int viewLoc = glGetUniformLocation(shaderProgram, "view");
        unsigned int projectionLoc = glGetUniformLocation(shaderProgram, "projection");
        glUniformMatrix4fv(modelLoc, 1, GL_FALSE, &model[0][0]);
        glUniformMatrix4fv(viewLoc, 1, GL_FALSE, &view[0][0]);
        glUniformMatrix4fv(projectionLoc, 1, GL_FALSE, &projection[0][0]);

        // Set up lighting
        unsigned int lightPosLoc = glGetUniformLocation(shaderProgram, "lightPos");
        unsigned int lightColorLoc = glGetUniformLocation(shaderProgram, "lightColor");
        unsigned int objectColorLoc = glGetUniformLocation(shaderProgram, "objectColor");
        glUniform3fv(lightPosLoc, 1, &lightPos[0]);
        glUniform3fv(lightColorLoc, 1, &lightColor[0]);
        glUniform3fv(objectColorLoc, 1, &objectColor[0]);

        // Set render mode
        unsigned int renderModeLoc = glGetUniformLocation(shaderProgram, "renderMode");
        glUniform1i(renderModeLoc, renderMode);

        // Draw the surface
        glUseProgram(shaderProgram);
        glBindVertexArray(VAO);
        glDrawElements(GL_TRIANGLE_STRIP, indices.size(), GL_UNSIGNED_INT, 0);

        glfwSwapBuffers(window);
        glfwPollEvents();
    }
}

void framebuffer_size_callback(GLFWwindow *window, int width, int height)
{
    glViewport(0, 0, width, height);
}

void processInput(GLFWwindow *window)
{
    if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
        glfwSetWindowShouldClose(window, true);

    float cameraSpeed = 0.05f;
    if (glfwGetKey(window, GLFW_KEY_W) == GLFW_PRESS)
        cameraPos += cameraSpeed * cameraFront;
    if (glfwGetKey(window, GLFW_KEY_S) == GLFW_PRESS)
        cameraPos -= cameraSpeed * cameraFront;
    if (glfwGetKey(window, GLFW_KEY_A) == GLFW_PRESS)
        cameraPos -= glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed;
    if (glfwGetKey(window, GLFW_KEY_D) == GLFW_PRESS)
        cameraPos += glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed;
}

void mouse_callback(GLFWwindow *window, double xpos, double ypos)
{
    if (firstMouse)
    {
        lastX = xpos;
        lastY = ypos;
        firstMouse = false;
    }

    float xoffset = xpos - lastX;
    float yoffset = lastY - ypos;
    lastX = xpos;
    lastY = ypos;

    float sensitivity = 0.1f;
    xoffset *= sensitivity;
    yoffset *= sensitivity;

    yaw += xoffset;
    pitch += yoffset;

    if (pitch > 89.0f)
        pitch = 89.0f;
    if (pitch < -89.0f)
        pitch = -89.0f;

    glm::vec3 direction;
    direction.x = cos(glm::radians(yaw)) * cos(glm::radians(pitch));
    direction.y = sin(glm::radians(pitch));
    direction.z = sin(glm::radians(yaw)) * cos(glm::radians(pitch));
    cameraFront = glm::normalize(direction);
}
void scroll_callback(GLFWwindow *window, double xoffset, double yoffset)
{
    fov -= (float)yoffset;
    if (fov < 1.0f)
        fov = 1.0f;
    if (fov > 45.0f)
        fov = 45.0f;
}