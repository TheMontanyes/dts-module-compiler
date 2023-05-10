import { container } from 'webpack';
import { normalizeOptions } from './normalize-options';
import { RequestOptions } from 'https';

export type MFOptions = ConstructorParameters<typeof container.ModuleFederationPlugin>[0];

export type NormalizeOptions = ReturnType<typeof normalizeOptions>;

/**
 * Объект конфигурации RemoteModuleTypesPlugin
 * */
export type ConfigOptions = {
  /** Объект или Promise возвращающий объект вида `{microfrontName: 'https://url'}` */
  manifestRemotes?: () => Promise<Record<string, string>> | Record<string, string>;
  /** Массив путей для дополнительных `d.ts` файлов */
  additionalFiles?: string[];
  /** Директория, для скачанных типов микрофронтов (default "@mf-types") */
  tsDownloadsTypesFolder?: string;
  /** Директория, для сборки типов (default "output.path") */
  tsBuildFolder?: string;
  /** Путь до конфига typescript `tsconfig.json` (default path.join(process.cwd(), 'tsconfig.json')) */
  tsConfigPath?: string;
  /** Конфиг для запроса инстанса https */
  requestOptions?: RequestOptions;
};
