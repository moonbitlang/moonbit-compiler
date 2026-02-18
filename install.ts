import { execSync } from 'child_process';
import {
  mkdirSync,
  readFileSync,
  writeFileSync,
  renameSync,
  chmodSync,
  existsSync,
  cpSync,
  rmSync,
} from 'fs';
import { join } from 'path';
import { homedir } from 'os';

/**
 * Executes a shell command synchronously and displays its output.
 * @param command - The command string to execute.
 * @param cwd - The working directory for the command (defaults to the current process's working directory).
 */
const run_command = (command: string, cwd: string = process.cwd()) => {
  console.log(`\n[CMD] Executing: ${command}\n`);
  try {
    // stdio: 'inherit' connects the child process's stdio to the parent's, showing live output.
    execSync(command, { stdio: 'inherit', cwd });
  } catch (error) {
    console.error(`\n[Error] Command failed: ${command}`);
    throw new Error(`Command failed: ${command}`);
  }
};

/**
 * Executes a shell command synchronously and return its output.
 * @param command - The command string to execute.
 * @param cwd - The working directory for the command (defaults to the current process's working directory).
 */
const exec = (command: string, cwd: string = process.cwd()) => {
  try {
    return execSync(command, { encoding: 'utf-8' })
  } catch (error) {
    console.error(`\n[Error] Command failed: ${command}`);
    throw new Error(`Command failed: ${command}`);
  }
}

/**
 * Synchronously checks if a single command exists in the system's PATH.
 * This function is BLOCKING and should only be used in scripts or at startup.
 * @param command - The name of the command to check (e.g., "git", "cargo").
 * @returns - A boolean, true if the command exists, otherwise false.
 */
const command_exists = (command: string): boolean => {
  try {
    // execSync will throw an error if the command is not found or fails to execute.
    // We redirect stdio to 'ignore' to prevent the command's output from cluttering the console.
    execSync(`${command} --version`, { stdio: 'ignore' });
    // If the above line does not throw an error, the command exists.
    return true;
  } catch (error) {
    // If we catch an error, it means the command likely does not exist.
    return false;
  }
}



/**
 * Downloads and extracts the MoonBit toolchain archive.
 */
const download_and_extract_toolchain = () => {
  console.log('\n[Step 1/6] Downloading and extracting MoonBit Wasm toolchain...');
  const downloadUrl =
    'https://github.com/moonbitlang/moonbit-compiler/releases/latest/download/moonbit-wasm.tar.gz';
  const archiveName = 'moonbit-wasm.tar.gz';
  run_command(`curl -fSL -O ${downloadUrl}`);
  run_command(`tar -zxvf ${archiveName}`);
};

/**
 * Builds and installs the 'moon' build system and 'moonrun' wasm runtime.
 * @param BIN_DIR - The path to the destination bin directory.
 * @param workDir - The current working directory inside the temp folder.
 */
const install_moon_build_system = (BIN_DIR: string, workDir: string) => {
  console.log("\n[Step 3/6] Building and installing 'moon' and 'moonrun'...");
  const moonVersion = readFileSync('moon_version', 'utf-8').trim();
  console.log(`moon version (commit SHA): ${moonVersion}`);

  const moonRepoDir = 'moon';
  run_command('git clone https://github.com/moonbitlang/moon');

  // Change to the moon repository directory
  const moonRepoPath = join(workDir, moonRepoDir);
  run_command(`git reset --hard "${moonVersion}"`, moonRepoPath);
  run_command('cargo build --release', moonRepoPath);

  // Copy build artifacts
  const moonBinPath = join(moonRepoPath, 'target', 'release', 'moon');
  const moonrunBinPath = join(moonRepoPath, 'target', 'release', 'moonrun');
  cpSync(moonBinPath, join(BIN_DIR, 'moon'));
  cpSync(moonrunBinPath, join(BIN_DIR, 'moonrun'));
  console.log(`'moon' and 'moonrun' have been copied to ${BIN_DIR}`);
};

/**
 * Installs the compiler (moonc), formatter (moonfmt), and info tool (mooninfo).
 * @param MOON_HOME - The path to the main .moon directory.
 * @param BIN_DIR - The path to the destination bin directory.
 */
const install_moonc_moonfmt_mooninfo = (MOON_HOME: string, BIN_DIR: string) => {
  console.log('\n[Step 4/6] Installing compiler, formatter, and info tools...');
  // Add shebang
  // TODO: how to gather `env` path dynamically?
  
  let envPath = exec('which env').trim();
  const shebang = `#!${envPath} -S node --stack-size=4096`;
  const jsFiles = ['moonc.js', 'moonfmt.js', 'mooninfo.js'];

  jsFiles.forEach((file) => {
    console.log(`Adding shebang to ${file}...`);
    const content = readFileSync(file, 'utf-8');
    writeFileSync(file, `${shebang}\n${content}`);
  });

  // Copy script files and asset files
  const assets = ['moonc.assets', 'moonfmt.assets', 'mooninfo.assets'];
  jsFiles.forEach((file) => cpSync(file, join(BIN_DIR, file)));
  assets.forEach((asset) =>
    cpSync(asset, join(BIN_DIR, asset), { recursive: true })
  );
  console.log('Copied JS scripts and asset directories.');

  // Copy lib and include directories
  cpSync('lib', join(MOON_HOME, 'lib'), { recursive: true });
  cpSync('include', join(MOON_HOME, 'include'), { recursive: true });
  console.log('Copied lib and include directories.');

  // Rename and add executable permissions
  jsFiles.forEach((file) => {
    const baseName = file.replace('.js', '');
    const oldPath = join(BIN_DIR, file);
    const newPath = join(BIN_DIR, baseName);
    renameSync(oldPath, newPath);
    chmodSync(newPath, 0o755); // 0o755 is rwxr-xr-x
    console.log(`Renamed ${file} to ${baseName} and made it executable.`);
  });
};

/**
 * Installs and bundles the core standard library.
 * @param MOON_HOME - The path to the main .moon directory.
 */
const install_core_library = (MOON_HOME: string, BIN_DIR: string) => {
  console.log('\n[Step 5/6] Installing and bundling the core standard library...');
  const libPath = join(MOON_HOME, 'lib');
  run_command(`tar -xf core.tar.gz --directory="${libPath}"`);
  const moonExecutable = join(BIN_DIR, 'moon');
  const corePath = join(libPath, 'core');
  const bundle_core = () => {
    const originalPath = process.env.PATH || '';
    const newPath = originalPath === '' ? BIN_DIR : `${BIN_DIR}:${originalPath}`;
    process.env.PATH = newPath;
    console.log(`Updated PATH for bundling: ${process.env.PATH}`);
    run_command(`${moonExecutable} bundle --target all`, corePath);
  };
  bundle_core();
};

/**
 * The main installation function.
 */
const main = async () => {
  // --- Setup ---
  console.log('Starting the installation of MoonBit Wasm toolchain...');
  const HOME = homedir();
  const MOON_HOME = join(HOME, '.moon');
  const BIN_DIR = join(MOON_HOME, 'bin');
  const TEMP_DIR = 'moonbit-wasm-temp'; // Temporary directory
  const originalCwd = process.cwd(); // Save original working directory
  let exit_code = 0;
  try {
    // check for required commands
    const requiredCommands = ['git', 'cargo'];
    for (const cmd of requiredCommands) {
      if (!command_exists(cmd)) {
        throw new Error(
          `Required command not found: ${cmd}. Please install it and ensure it's in your PATH.`
        );
      }
    }
    // --- Cleanup and setup temporary directory ---
    if (existsSync(TEMP_DIR)) {
      console.log(`Cleaning up existing temporary directory: ${TEMP_DIR}`);
      rmSync(TEMP_DIR, { recursive: true, force: true });
    }
    mkdirSync(TEMP_DIR);
    process.chdir(TEMP_DIR); // Enter the temporary directory
    const workDir = process.cwd();
    console.log(`Created temporary working directory: ${workDir}`);

    // --- Execute Installation Steps ---
    download_and_extract_toolchain();

    console.log(`\n[Step 2/6] Setting up MOON_HOME directory: ${MOON_HOME}...`);

    if (existsSync(MOON_HOME)) {
      console.log(`clean up MOON_HOME directory: ${MOON_HOME}`);
      rmSync(MOON_HOME, { recursive: true, force: true });
    }
    mkdirSync(MOON_HOME, { recursive: true });
    mkdirSync(BIN_DIR, { recursive: true });

    install_moon_build_system(BIN_DIR, workDir);
    install_moonc_moonfmt_mooninfo(MOON_HOME, BIN_DIR);
    install_core_library(MOON_HOME, BIN_DIR);

    console.log('\n[Step 6/6] Finalizing installation...');
    console.log('\n✅ MoonBit Wasm toolchain installed successfully!');
    console.log('\n--- IMPORTANT: Final Step ---\n');
    console.log(
      `Please add the following line to your shell configuration file (e.g., ~/.bashrc, ~/.zshrc, ~/.config/fish/config.fish):`
    );
    console.log('\nbash/zsh:  export PATH="$HOME/.moon/bin:$PATH"\n');
    console.log(
      '\nfish:  fish_add_path "$HOME/.moon/bin"\n'
    );
    console.log(
      "After adding it, please restart your terminal to apply the changes."
    );
  } catch (error) {
    if (error instanceof Error) {
      console.error(`\n${error.message}`);
    }
    console.error('\n❌ Installation failed. Please check the error messages above.');
    exit_code = 1;
  } finally {
    // --- Cleanup ---
    process.chdir(originalCwd); // Ensure we return to the original directory
    if (existsSync(TEMP_DIR)) {
      console.log(`\nCleaning up temporary directory: ${TEMP_DIR}`);
      rmSync(TEMP_DIR, { recursive: true, force: true });
    }
    process.exit(exit_code);
  }
};

// Run the main function
main();

