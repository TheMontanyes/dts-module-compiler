import https from 'node:https';
import fs from 'node:fs';
import path from 'node:path';
import chalk from 'chalk';

const download = async ({
  dirDest,
  filename,
  requestOptions,
}: {
  dirDest: string;
  filename: string;
  requestOptions: string | URL | https.RequestOptions;
}) => {
  const dest = path.join(dirDest, filename);

  const alreadyExist = fs.existsSync(dest);
  let currentFile: Buffer;

  if (alreadyExist) {
    currentFile = fs.readFileSync(dest);
  }

  const file = fs.createWriteStream(dest);

  const handleFailedFile = () => {
    if (alreadyExist) {
      fs.writeFileSync(dest, currentFile, { encoding: 'utf-8' });
      return;
    }

    if (fs.existsSync(dest)) {
      fs.rmSync(path.dirname(dest), { recursive: true });
    }
  };

  await new Promise<void>((resolve) => {
    const request = https
      .request(requestOptions, (response) => {
        if (response.statusCode !== 200) {
          console.error(chalk.yellow(`RemoteModuleTypesPlugin: [WARN] File ${filename} not found`));
          resolve();

          handleFailedFile();

          file.close();
        } else {
          file.on('finish', function () {
            file.close();
            resolve();
          });
        }

        response.pipe(file);
      })
      .on('error', (err) => {
        handleFailedFile();
        resolve();
        console.error(chalk.red(`RemoteModuleTypesPlugin: [ERROR] ${err}`));
      });

    request.end();
  });
};

export { download };
